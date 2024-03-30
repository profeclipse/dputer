Set-Variable -Name TMPDIR -Value $PSScriptRoot\tmp

if (!([System.IO.Directory]::Exists($TMPDIR))) {
    mkdir $TMPDIR
}

$machines = ('cbmbasic1','cbmbasic2','kbdbasic','osi','kb9','applesoft','microtan','aim65','sym1')
foreach ($machine in $machines) {
    $machine
    ca65 -D ${machine} msbasic.s -o tmp/${machine}.o
    ld65 -C ${machine}.cfg tmp/${machine}.o -o tmp/${machine}.bin -Ln tmp/${machine}.lbl
}

