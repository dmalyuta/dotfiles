# Put the Virtual Display Driver at a given mode and make it the primary
# display, so the desktop Looking Glass captures is the one applications
# actually open on.
param(
    [int]$Width  = 2560,
    [int]$Height = 1440,
    [int]$Hz     = 60
)

$ErrorActionPreference = 'Stop'

# A scheduled task hands back only an exit code, so the script has to keep its
# own record of what happened.
$logPath = 'C:\Users\Public\set-display.log'
try { Stop-Transcript | Out-Null } catch { }
Start-Transcript -Path $logPath -Force | Out-Null
trap { "FAILED: $_" | Out-String | Write-Output; Stop-Transcript | Out-Null; exit 1 }

Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;

[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
public struct DISPLAY_DEVICE
{
    public int  cb;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]  public string DeviceName;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)] public string DeviceString;
    public int  StateFlags;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)] public string DeviceID;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)] public string DeviceKey;
}

[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
public struct DEVMODE
{
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] public string dmDeviceName;
    public short dmSpecVersion;
    public short dmDriverVersion;
    public short dmSize;
    public short dmDriverExtra;
    public int   dmFields;
    public int   dmPositionX;
    public int   dmPositionY;
    public int   dmDisplayOrientation;
    public int   dmDisplayFixedOutput;
    public short dmColor;
    public short dmDuplex;
    public short dmYResolution;
    public short dmTTOption;
    public short dmCollate;
    [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] public string dmFormName;
    public short dmLogPixels;
    public int   dmBitsPerPel;
    public int   dmPelsWidth;
    public int   dmPelsHeight;
    public int   dmDisplayFlags;
    public int   dmDisplayFrequency;
    public int   dmICMMethod;
    public int   dmICMIntent;
    public int   dmMediaType;
    public int   dmDitherType;
    public int   dmReserved1;
    public int   dmReserved2;
    public int   dmPanningWidth;
    public int   dmPanningHeight;
}

public static class Disp
{
    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern bool EnumDisplayDevices(string dev, uint num, ref DISPLAY_DEVICE d, uint flags);

    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern bool EnumDisplaySettings(string dev, int mode, ref DEVMODE dm);

    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern int ChangeDisplaySettingsEx(string dev, ref DEVMODE dm, IntPtr wnd, uint flags, IntPtr p);

    [DllImport("user32.dll")]
    public static extern int ChangeDisplaySettingsEx(IntPtr dev, IntPtr dm, IntPtr wnd, uint flags, IntPtr p);
}
"@

$ENUM_CURRENT_SETTINGS = -1
$CDS_UPDATEREGISTRY    = 0x00000001
$CDS_SET_PRIMARY       = 0x00000010
$CDS_NORESET           = 0x10000000
$DM_POSITION           = 0x00000020
$DM_PELSWIDTH          = 0x00080000
$DM_PELSHEIGHT         = 0x00100000
$DM_DISPLAYFREQUENCY   = 0x00400000

# Locate the VDD adapter among the attached displays.
$target = $null
for ($i = 0; $i -lt 16; $i++) {
    $dd = New-Object DISPLAY_DEVICE
    $dd.cb = [Runtime.InteropServices.Marshal]::SizeOf($dd)
    # [NullString]::Value, not $null: PowerShell turns $null into an empty
    # string for a [string] parameter, and EnumDisplayDevices("") fails --
    # only a real NULL means "enumerate the adapters".
    if (-not [Disp]::EnumDisplayDevices([NullString]::Value, $i, [ref]$dd, 0)) { break }
    if (($dd.StateFlags -band 0x1) -eq 0) { continue }   # not attached to desktop
    "found: {0}  {1}" -f $dd.DeviceName, $dd.DeviceString
    if ($dd.DeviceString -match 'Virtual Display Driver') { $target = $dd.DeviceName }
}
if (-not $target) { throw "Virtual Display Driver is not attached to the desktop" }

$dm = New-Object DEVMODE
$dm.dmSize = [int16][Runtime.InteropServices.Marshal]::SizeOf($dm)
if (-not [Disp]::EnumDisplaySettings($target, $ENUM_CURRENT_SETTINGS, [ref]$dm)) {
    throw "could not read current mode for $target"
}

$dm.dmPelsWidth        = $Width
$dm.dmPelsHeight       = $Height
$dm.dmDisplayFrequency = $Hz
$dm.dmPositionX        = 0
$dm.dmPositionY        = 0
$dm.dmFields = $DM_PELSWIDTH -bor $DM_PELSHEIGHT -bor $DM_DISPLAYFREQUENCY -bor $DM_POSITION

# NORESET stages the change; the null commit below applies it, which is the
# only way to move the primary flag and reposition in one consistent step.
$rc = [Disp]::ChangeDisplaySettingsEx($target, [ref]$dm, [IntPtr]::Zero,
        ($CDS_UPDATEREGISTRY -bor $CDS_SET_PRIMARY -bor $CDS_NORESET), [IntPtr]::Zero)
"staged $target at ${Width}x${Height}@${Hz}: rc=$rc"
if ($rc -ne 0) { throw "ChangeDisplaySettingsEx returned $rc" }

$rc = [Disp]::ChangeDisplaySettingsEx([IntPtr]::Zero, [IntPtr]::Zero, [IntPtr]::Zero, 0, [IntPtr]::Zero)
"commit: rc=$rc"
if ($rc -ne 0) { throw "commit returned $rc" }

Stop-Transcript | Out-Null
exit 0
