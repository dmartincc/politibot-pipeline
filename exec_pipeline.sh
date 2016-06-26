echo "Download data"
wget -O scripts/kiko_input.csv http://nocdn.politikon.es/elecciones/kiko_input.csv

echo "Run model"
Rscript scripts/1_run_test.r

echo "Uploading file"
bash Dropbox-Uploader/dropbox_uploader.sh upload output.csv output.csv