param (
    [parameter(Mandatory=$false)]
    [int] $BuildThreadCount=0,
    [parameter(Mandatory=$false)]
    [String] $BuildType="MinSizeRel"
)

### CONFIGURATION START ###
$ErrorActionPreference = "Stop"

if ( $BuildThreadCount -eq 0 ) {
    $ComputerSystem = Get-CimInstance -class Win32_ComputerSystem
    $BuildThreadCount = $ComputerSystem.NumberOfLogicalProcessors - 1
    if ( $BuildThreadCount -le 0) {
        $BuildThreadCount = 1
    }
}

Write-Output "Build thread count: $BuildThreadCount"
Write-Output "Build type: $BuildType"

$WorkDir = $PSScriptRoot
$BuildDir = "$WorkDir/build/desktop/"

### CONFIGURATION END ###

$LibraryDir = "$WorkDir/physx/"

cp $WorkDir/vc16win64-clawed.xml $LibraryDir/physx/buildtools/presets/public/

pushd $LibraryDir/physx/
cmd /C generate_projects.bat vc16win64-clawed
popd

pushd $LibraryDir/physx/compiler/vc16win64/
cmake --build . --config release
popd

md $BuildDir -Force | Out-Null
pushd $BuildDir

cmake -G "Visual Studio 16 2019" -A x64 -Thost=x64 `
  "$WorkDir"

cmake --build "$BuildDir" --config $BuildType --parallel $BuildThreadCount

cp $BuildDir/$BuildType/physx.clawed.dll $BuildDir/
cp $LibraryDir/physx/bin/win.x86_64.vc142.md/release/PhysXGpu_64.dll $BuildDir/

popd
