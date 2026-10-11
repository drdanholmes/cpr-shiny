# Download CRAN's R for Windows and silently install it into electron/r-win,
# then strip parts not needed at runtime to shrink the installer.
param(
  [string]$RVersion = "4.5.3"
)
$ErrorActionPreference = "Stop"
$ProgressPreference = "SilentlyContinue"   # makes Invoke-WebRequest much faster

$electronDir = Split-Path -Parent $PSScriptRoot
$target = Join-Path $electronDir "r-win"
$installer = Join-Path $env:RUNNER_TEMP "R-$RVersion-win.exe"
if (-not $env:RUNNER_TEMP) { $installer = Join-Path $env:TEMP "R-$RVersion-win.exe" }

# The current release lives in base/, older ones in base/old/<version>/.
$urls = @(
  "https://cloud.r-project.org/bin/windows/base/R-$RVersion-win.exe",
  "https://cloud.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe"
)
$downloaded = $false
foreach ($u in $urls) {
  try {
    Write-Host "Downloading $u"
    Invoke-WebRequest -Uri $u -OutFile $installer
    $downloaded = $true
    break
  } catch {
    Write-Host "  not found"
  }
}
if (-not $downloaded) { throw "Could not download R $RVersion" }

if (Test-Path $target) { Remove-Item $target -Recurse -Force }

Write-Host "Installing R $RVersion into $target"
$p = Start-Process -FilePath $installer -Wait -PassThru -ArgumentList @(
  "/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART", "/CURRENTUSER",
  "/NOICONS", "/DIR=`"$target`""
)
if ($p.ExitCode -ne 0) { throw "R installer exited with code $($p.ExitCode)" }
if (-not (Test-Path (Join-Path $target "bin\Rscript.exe"))) { throw "Rscript.exe missing after install" }

# Not needed at runtime: manuals, headers, test suites, uninstaller.
foreach ($d in @("doc\manual", "include", "tests", "Tcl")) {
  $full = Join-Path $target $d
  if (Test-Path $full) { Remove-Item $full -Recurse -Force }
}
Get-ChildItem $target -Filter "unins*" | Remove-Item -Force

Write-Host "R $RVersion ready in $target"
