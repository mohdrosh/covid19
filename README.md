$f = "$env:USERPROFILE\Downloads\scale_COM6_polling.exe"
$b = [IO.File]::ReadAllBytes($f)
$o = [Text.Encoding]::ASCII.GetBytes("COM5")
$n = [Text.Encoding]::ASCII.GetBytes("COM6")
for ($i = 0; $i -lt $b.Length - 3; $i++) {
    if ($b[$i] -eq $o[0] -and $b[$i+1] -eq $o[1] -and $b[$i+2] -eq $o[2] -and $b[$i+3] -eq $o[3]) {
        $b[$i+3] = $n[3]; Write-Host "Patched at $i"
    }
}
[IO.File]::WriteAllBytes($f, $b)
Write-Host "Done - now run the exe"
