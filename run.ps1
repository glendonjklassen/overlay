<#
.SYNOPSIS
    Drives cabal inside WSL against this project, from Windows.

.DESCRIPTION
    Forwards arguments to `cabal` running in the default WSL distro, with the
    working directory set to this folder. No arguments defaults to `run overlay`.

    Build artifacts are written to the Linux filesystem (~/.cache/overlay-dist)
    instead of ./dist-newstyle, because compiling onto /mnt/c through WSL's 9P
    file bridge is roughly an order of magnitude slower.

.EXAMPLE
    .\run.ps1                  # cabal run overlay
    .\run.ps1 build            # cabal build
    .\run.ps1 repl             # cabal repl
    .\run.ps1 clean            # cabal clean
#>
param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$CabalArgs
)

if (-not $CabalArgs -or $CabalArgs.Count -eq 0) {
    $CabalArgs = @('run', 'overlay')
}

# Regenerate overlay.cabal from package.yaml (hpack) before invoking cabal.
$builddir = '$HOME/.cache/overlay-dist'
$cmd = "{ [ ! -f package.yaml ] || hpack; } && cabal --builddir=`"$builddir`" " + ($CabalArgs -join ' ')

wsl --cd "$PSScriptRoot" -e bash -lc $cmd
exit $LASTEXITCODE
