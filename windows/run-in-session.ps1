# Run a PowerShell script inside the logged-on user's interactive session.
#
# The QEMU guest agent runs as SYSTEM in session 0, which has its own headless
# desktop. Anything that touches the real display -- EnumDisplayDevices,
# ChangeDisplaySettingsEx, most shell APIs -- sees nothing from there. A
# scheduled task with an Interactive principal is the supported way across the
# session boundary; it runs as the user, on their desktop.
param(
    [Parameter(Mandatory = $true)][string]$ScriptPath,
    [string]$ScriptArgs     = '',
    [string]$UserId         = '',
    [int]   $TimeoutSeconds = 120
)

$ErrorActionPreference = 'Stop'

if (-not $UserId) {
    # Win32_ComputerSystem.UserName is the user logged on at the console, which
    # is the one whose desktop we want. Picking the owner of any explorer.exe
    # is wrong: Windows runs its own copies under helper accounts such as
    # WsiAccount for the sign-in UI, and those have no usable desktop.
    $UserId = (Get-CimInstance Win32_ComputerSystem).UserName
    if (-not $UserId) { throw 'No interactive session: nobody is logged on at the console.' }
}
"Running as   : $UserId"

$taskName = 'RunInSession'
# C:\Users\Public is writable by every interactive user; the SYSTEM
# context's own TEMP is not, and the task runs as the user.
$logPath  = 'C:\Users\Public\run-in-session.log'
Remove-Item $logPath -ErrorAction SilentlyContinue

# *> redirects every stream, so a failure inside the task still leaves a trace;
# the task itself reports only an exit code.
# -File rather than -Command: one less layer of quoting to survive the trip
# through Task Scheduler, which is where this previously came apart.
$action = New-ScheduledTaskAction -Execute 'powershell.exe' `
    -Argument "-NoProfile -NonInteractive -ExecutionPolicy Bypass -File `"$ScriptPath`" $ScriptArgs"
$principal = New-ScheduledTaskPrincipal -UserId $UserId -LogonType Interactive -RunLevel Highest

Register-ScheduledTask -TaskName $taskName -Action $action -Principal $principal -Force | Out-Null
try {
    Start-ScheduledTask -TaskName $taskName
    $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
    do {
        Start-Sleep -Milliseconds 500
        $info = Get-ScheduledTask -TaskName $taskName | Get-ScheduledTaskInfo
        $state = (Get-ScheduledTask -TaskName $taskName).State
    } while ($state -eq 'Running' -and (Get-Date) -lt $deadline)

    "task state    : $state"
    "last result   : 0x{0:X}" -f $info.LastTaskResult
    if (Test-Path $logPath) {
        '=== script output ==='
        Get-Content $logPath
    } else {
        '(no output captured)'
    }
} finally {
    Unregister-ScheduledTask -TaskName $taskName -Confirm:$false -ErrorAction SilentlyContinue
}
