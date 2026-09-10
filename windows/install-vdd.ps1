# Install the Virtual Display Driver and create the device node it binds to.
#
# The driver's only hardware ID is Root\MttVDD, meaning there is no physical
# device for Windows to match it against; the node has to be created first.
# pnputil cannot do that -- it only binds drivers to devices that already
# exist -- and devcon.exe, which can, ships only inside a 70 MB download. The
# three SetupAPI calls below are what devcon does internally.
param(
    [Parameter(Mandatory = $true)][string]$InfPath,
    [string]$HardwareId = 'Root\MttVDD'
)

$ErrorActionPreference = 'Stop'
$logPath = 'C:\Users\Public\install-vdd.log'
try { Stop-Transcript | Out-Null } catch { }
Start-Transcript -Path $logPath -Force | Out-Null
trap { "FAILED: $_" | Out-String | Write-Output; Stop-Transcript | Out-Null; exit 1 }

if (-not (Test-Path $InfPath)) { throw "no such INF: $InfPath" }
$InfPath = (Resolve-Path $InfPath).Path

# The catalog is signed by a publisher Windows trusts but has not been told to
# accept drivers from. Without this the install prompts, and a prompt on a
# machine being configured over the guest agent simply hangs.
$cat = [IO.Path]::ChangeExtension($InfPath, '.cat')
if (Test-Path $cat) {
    $sig = Get-AuthenticodeSignature $cat
    if ($sig.Status -ne 'Valid') { throw "catalog signature is $($sig.Status)" }
    $store = New-Object Security.Cryptography.X509Certificates.X509Store('TrustedPublisher', 'LocalMachine')
    $store.Open('ReadWrite')
    $store.Add($sig.SignerCertificate)
    $store.Close()
    "trusted publisher: $($sig.SignerCertificate.Subject)"
}

Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;

[StructLayout(LayoutKind.Sequential)]
public struct SP_DEVINFO_DATA
{
    public int    cbSize;
    public Guid   ClassGuid;
    public int    DevInst;
    public IntPtr Reserved;
}

public static class SetupApi
{
    [DllImport("setupapi.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    public static extern IntPtr SetupDiCreateDeviceInfoList(ref Guid classGuid, IntPtr parent);

    [DllImport("setupapi.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    public static extern bool SetupDiCreateDeviceInfoW(IntPtr devInfoSet, string name,
        ref Guid classGuid, string description, IntPtr parent, int flags,
        ref SP_DEVINFO_DATA devInfoData);

    [DllImport("setupapi.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    public static extern bool SetupDiSetDeviceRegistryPropertyW(IntPtr devInfoSet,
        ref SP_DEVINFO_DATA devInfoData, int property, byte[] buffer, int bufferSize);

    [DllImport("setupapi.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    public static extern bool SetupDiCallClassInstaller(int installFunction,
        IntPtr devInfoSet, ref SP_DEVINFO_DATA devInfoData);

    [DllImport("setupapi.dll", SetLastError = true)]
    public static extern bool SetupDiDestroyDeviceInfoList(IntPtr devInfoSet);

    [DllImport("newdev.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    public static extern bool UpdateDriverForPlugAndPlayDevicesW(IntPtr parent,
        string hardwareId, string infPath, int flags, out bool rebootRequired);
}
"@

$DISPLAY_CLASS      = [Guid]'4D36E968-E325-11CE-BFC1-08002BE10318'
$DICD_GENERATE_ID   = 0x00000001
$SPDRP_HARDWAREID   = 0x00000001
$DIF_REGISTERDEVICE = 0x00000019
$INSTALLFLAG_FORCE  = 0x00000001
$INVALID_HANDLE     = [IntPtr]::new(-1)

# Already there from an earlier run? Creating a second node would give the
# guest two virtual monitors.
$existing = Get-PnpDevice -Class Display -ErrorAction SilentlyContinue |
    Where-Object { $_.InstanceId -like 'ROOT\DISPLAY\*' -and $_.FriendlyName -match 'Virtual Display' }
if ($existing) {
    "device node already exists: $($existing.InstanceId)"
} else {
    $guid = $DISPLAY_CLASS
    $set = [SetupApi]::SetupDiCreateDeviceInfoList([ref]$guid, [IntPtr]::Zero)
    if ($set -eq $INVALID_HANDLE) { throw "SetupDiCreateDeviceInfoList: $([ComponentModel.Win32Exception]::new([Runtime.InteropServices.Marshal]::GetLastWin32Error()).Message)" }
    try {
        $dev = New-Object SP_DEVINFO_DATA
        $dev.cbSize = [Runtime.InteropServices.Marshal]::SizeOf($dev)
        if (-not [SetupApi]::SetupDiCreateDeviceInfoW($set, 'Display', [ref]$guid, $null,
                [IntPtr]::Zero, $DICD_GENERATE_ID, [ref]$dev)) {
            throw "SetupDiCreateDeviceInfoW: $([ComponentModel.Win32Exception]::new([Runtime.InteropServices.Marshal]::GetLastWin32Error()).Message)"
        }

        # SPDRP_HARDWAREID is REG_MULTI_SZ: the id, then two terminating nulls.
        $bytes = [Text.Encoding]::Unicode.GetBytes($HardwareId + "`0`0")
        if (-not [SetupApi]::SetupDiSetDeviceRegistryPropertyW($set, [ref]$dev,
                $SPDRP_HARDWAREID, $bytes, $bytes.Length)) {
            throw "SetupDiSetDeviceRegistryPropertyW: $([ComponentModel.Win32Exception]::new([Runtime.InteropServices.Marshal]::GetLastWin32Error()).Message)"
        }

        if (-not [SetupApi]::SetupDiCallClassInstaller($DIF_REGISTERDEVICE, $set, [ref]$dev)) {
            throw "SetupDiCallClassInstaller: $([ComponentModel.Win32Exception]::new([Runtime.InteropServices.Marshal]::GetLastWin32Error()).Message)"
        }
        "created device node for $HardwareId"
    } finally {
        [void][SetupApi]::SetupDiDestroyDeviceInfoList($set)
    }
}

# Binds the driver package to every device with that hardware id, which is now
# the node above.
$reboot = $false
if (-not [SetupApi]::UpdateDriverForPlugAndPlayDevicesW([IntPtr]::Zero, $HardwareId,
        $InfPath, $INSTALLFLAG_FORCE, [ref]$reboot)) {
    throw "UpdateDriverForPlugAndPlayDevices: $([ComponentModel.Win32Exception]::new([Runtime.InteropServices.Marshal]::GetLastWin32Error()).Message)"
}
"driver installed from $InfPath (reboot required: $reboot)"

$d = Get-PnpDevice -Class Display | Where-Object { $_.InstanceId -like 'ROOT\DISPLAY\*' }
"device status: $($d.Status) $($d.FriendlyName) $($d.InstanceId)"

Stop-Transcript | Out-Null
exit 0
