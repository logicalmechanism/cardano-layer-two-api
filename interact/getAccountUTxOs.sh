pkh='payload=0199fc1a514a232b29a18db7635e6650b8cf65b65da83491532cb928dd5b7e1c5c4098962aca148e279363cc09e681268a5995d9d226e93b5d'
# echo $pkh
# exit

curl -X POST http://localhost:8000/entries/getUTxOs/ \
-H "Authorization: Token f998b84793d3d56b42b7fad786dfd53e15112ace" \
--data "${pkh}"

