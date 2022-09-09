from django.db import models

class Datum(models.Model):
    datumHash = models.CharField(max_length=128)
    data      = models.JSONField()

class Redeemer(models.Model):
    data = models.JSONField()

class Token(models.Model):
    pid  = models.CharField(max_length=128)
    name = models.CharField(max_length=128)

class Value(models.Model):
    token  = models.ForeignKey(Token, on_delete=models.CASCADE)
    amount = models.IntegerField()

class UTxO(models.Model):
    txId     = models.CharField(max_length=128, unique=True)
    value    = models.ManyToManyField(Value)
    datum    = models.ManyToManyField(Datum)

class Account(models.Model):
    pkh = models.CharField(max_length=128, unique=True)

class Entry(models.Model):
    account = models.ForeignKey(Account, on_delete=models.CASCADE)
    utxo    = models.ForeignKey(UTxO, on_delete=models.CASCADE)

class Task(models.Model):
    number  = models.IntegerField(unique=True)
    cbor    = models.TextField()

