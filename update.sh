date_path=$(date +%Y-%m-%d)
currenttime=$(date +%H)
if [[ "$currenttime" -ge  "12" ]] && [[ "$currenttime" -le "20" ]]
then
	/usr/local/bin/Rscript auto_update.R euro
	git add *
	git commit -m "Euro Cup Tournament Update ${date_path}" 
fi	
if [[ "$currenttime" > "20" ]]
then
	/usr/local/bin/Rscript auto_update.R copa
	git add *
	git commit -m "Copa America Tournament Update ${date_path}" 
fi
git push -u origin main
