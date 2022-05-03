from django.db import models

class Token(models.Model):
    pid = models.CharField(max_length=128)
    name = models.CharField(max_length=128)

class Value(models.Model):
    token = models.ForeignKey(Token, on_delete=models.CASCADE)
    amount = models.IntegerField()

class UTxO(models.Model):
    txId = models.CharField(max_length=128, unique=True)
    value = models.ForeignKey(Value, on_delete=models.CASCADE)

class Account(models.Model):
    pkh = models.CharField(max_length=128, unique=True)

class Entry(models.Model):
    account = models.ForeignKey(Account, on_delete=models.CASCADE)
    utxo = models.ForeignKey(UTxO, on_delete=models.CASCADE)

class Task(models.Model):
    cbor = models.TextField()
