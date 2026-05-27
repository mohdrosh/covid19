$b = [IO.File]::ReadAllBytes($f); for($i=0;$i-lt$b.Length-3;$i++){if($b[$i]-eq67-and$b[$i+1]-eq79-and$b[$i+2]-eq77-and$b[$i+3]-eq53){$b[$i+3]=54;break}}
