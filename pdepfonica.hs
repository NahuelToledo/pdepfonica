type NumeroTelefonico = String
type Saldo = Float
type Cliente = (NumeroTelefonico, Saldo)

alf :: Cliente
alf = ("66660000", 5)
debi :: Cliente
debi = ("77770000", -20)

numeroTelefonico :: Cliente -> NumeroTelefonico
-- numeroTelefonico (numero, _) = numero
numeroTelefonico = fst

saldo :: Cliente -> Saldo
-- saldo (_, saldo) = saldo
saldo = snd

esNuevo :: Cliente -> Bool
esNuevo = (== '7') . head . numeroTelefonico

esMoroso :: Cliente -> Bool
esMoroso = esNegativo . saldo

esNegativo = (<0)


disminuirSaldo :: Cliente -> Saldo -> Cliente
-- disminuirSaldo cliente saldoADisminuir =
--     (numeroTelefonico cliente, saldo cliente - saldoADisminuir)

disminuirSaldo (numero, saldo) saldoADisminuir =
    (numero, saldo - saldoADisminuir)
    