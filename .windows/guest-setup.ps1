# Guest-side setup for the Windows VM.
#
# Run by make_windows_vm.sh through the QEMU guest agent. Every step checks for
# what it creates and skips it if it is already there, so this is safe to run
# again after installing new software or a Windows feature update.
#
# Written by make_windows_vm.sh; edit that instead of this.

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'   # progress bars corrupt agent output

function Step($msg) { Write-Output "  $msg" }

# --- The shared folder ------------------------------------------------------
# virtiofs needs two halves in the guest: the driver, which arrives with the
# virtio guest tools, and WinFsp, which is what lets Windows mount a userspace
# filesystem at all. Without WinFsp the service is present but cannot start.
$winfsp = 'C:\Program Files (x86)\WinFsp\bin'
if (Test-Path $winfsp) {
    Step 'WinFsp already installed.'
} else {
    Step 'Installing WinFsp (needed to mount the shared folder)...'
    $msi = Join-Path $env:TEMP 'winfsp.msi'
    Invoke-WebRequest -Uri '{{WINFSP_URL}}' -OutFile $msi -UseBasicParsing
    $p = Start-Process msiexec.exe -Wait -PassThru `
        -ArgumentList @('/i', "`"$msi`"", '/qn', '/norestart')
    Remove-Item $msi -ErrorAction SilentlyContinue
    if ($p.ExitCode -ne 0) { throw "WinFsp installer exited $($p.ExitCode)" }
    Step 'WinFsp installed.'
}

$svc = Get-Service VirtioFsSvc -ErrorAction SilentlyContinue
if ($null -eq $svc) {
    Write-Warning 'VirtioFsSvc is missing. Install the virtio guest tools from the virtio-win CD.'
} else {
    if ($svc.StartType -ne 'Automatic') {
        Set-Service VirtioFsSvc -StartupType Automatic
        Step 'VirtioFsSvc set to start automatically.'
    }
    if ($svc.Status -ne 'Running') {
        Start-Service VirtioFsSvc
        Step 'VirtioFsSvc started.'
    } else {
        Step 'VirtioFsSvc already running.'
    }
}

# --- RemoteApp --------------------------------------------------------------
# Windows Pro refuses to serve individual application windows unless the
# allow-list is switched off. This does not widen access: anyone who can log in
# over RDP already has a full desktop, and therefore every program on it.
$ts = 'HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\TSAppAllowList'
if (-not (Test-Path $ts)) { New-Item -Path $ts -Force | Out-Null }
if ((Get-ItemProperty $ts -Name fDisabledAllowList -EA SilentlyContinue).fDisabledAllowList -eq 1) {
    Step 'RemoteApp already enabled.'
} else {
    New-ItemProperty $ts -Name fDisabledAllowList -PropertyType DWord -Value 1 -Force | Out-Null
    Step 'RemoteApp enabled.'
}

# Make sure RDP itself is on, in case a feature update reset it.
$tsRoot = 'HKLM:\System\CurrentControlSet\Control\Terminal Server'
if ((Get-ItemProperty $tsRoot).fDenyTSConnections -ne 0) {
    Set-ItemProperty $tsRoot -Name fDenyTSConnections -Value 0
    Enable-NetFirewallRule -DisplayGroup 'Remote Desktop' -EA SilentlyContinue
    Step 'RDP re-enabled.'
}

# --- Sessions ---------------------------------------------------------------
# Windows 11 Pro allows one interactive session. If the machine signs itself in
# at the console on every boot, that session is always taken and RemoteApp
# cannot have it. Booting to the sign-in screen leaves it free, and the console
# is still there through virt-viewer or Looking Glass whenever it is wanted.
$wl = 'HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon'
if ((Get-ItemProperty $wl).AutoAdminLogon -eq '0') {
    Step 'Automatic sign-in already off.'
} else {
    Set-ItemProperty $wl -Name AutoAdminLogon -Value '0'
    Remove-ItemProperty $wl -Name DefaultPassword -EA SilentlyContinue
    Step 'Automatic sign-in turned off, so RemoteApp can take the session.'
}

# --- Report what is installed ----------------------------------------------
# The host uses this to decide which launcher entries to create. Printed as one
# tab-separated line per program so the shell side does not have to parse
# anything cleverer than cut.
Write-Output '---PROGRAMS---'
# Several paths per program on purpose: vendors move things between versions,
# and a program that is installed but looked for in last year's directory is
# indistinguishable here from one that is not installed at all.
$candidates = @(
    @{ Name = 'Excel';      Paths = @('Microsoft Office\root\Office16\EXCEL.EXE',
                                      'Microsoft Office\Office16\EXCEL.EXE') }
    @{ Name = 'Word';       Paths = @('Microsoft Office\root\Office16\WINWORD.EXE',
                                      'Microsoft Office\Office16\WINWORD.EXE') }
    @{ Name = 'PowerPoint'; Paths = @('Microsoft Office\root\Office16\POWERPNT.EXE',
                                      'Microsoft Office\Office16\POWERPNT.EXE') }
    @{ Name = 'Outlook';    Paths = @('Microsoft Office\root\Office16\OUTLOOK.EXE',
                                      'Microsoft Office\Office16\OUTLOOK.EXE') }
    @{ Name = 'OneNote';    Paths = @('Microsoft Office\root\Office16\ONENOTE.EXE',
                                      'Microsoft Office\Office16\ONENOTE.EXE') }
    @{ Name = 'Access';     Paths = @('Microsoft Office\root\Office16\MSACCESS.EXE',
                                      'Microsoft Office\Office16\MSACCESS.EXE') }
    # Newer builds ship as PDF-XChange\PDF Editor\PXCEditor.exe; the older
    # Tracker Software layout is kept for machines that still have it.
    @{ Name = 'PDF-XChange Editor'; Paths = @('PDF-XChange\PDF Editor\PXCEditor.exe',
                                              'Tracker Software\PDF Editor\PDFXEdit.exe') }
)

$roots = @($env:ProgramFiles, ${env:ProgramFiles(x86)}) | Where-Object { $_ }

foreach ($c in $candidates) {
    $found = $null
    foreach ($root in $roots) {
        foreach ($rel in $c.Paths) {
            $full = Join-Path $root $rel
            if (Test-Path $full) { $found = $full; break }
        }
        if ($found) { break }
    }
    if ($found) { Write-Output ("{0}`t{1}" -f $c.Name, $found) }
}

# Always useful, and always present, so they double as a way to prove the
# whole path works before any real software is installed.
Write-Output ("{0}`t{1}" -f 'Notepad', "$env:SystemRoot\System32\notepad.exe")
Write-Output ("{0}`t{1}" -f 'File Explorer', "$env:SystemRoot\explorer.exe")
Write-Output '---END---'
