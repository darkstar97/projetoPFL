numeralParaExtenso :: Integer -> String
numeralParaExtenso n
  | n < 0 = "menos " ++ numeralParaExtenso(n*(-1))
  | n == 0 = "zero"
  | n == 1 = "um"
  | n == 2 = "dois"
  | n == 3 = "tres"
  | n == 4 = "quatro"
  | n == 5 = "cinco"
  | n == 6 = "seis"
  | n == 7 = "sete"
  | n == 8 = "oito"
  | n == 9 = "nove"
  | n == 10 = "dez"
  | n == 11 = "onze"
  | n == 12 = "doze"
  | n == 13 = "treze"
  | n == 14 = "quatorze"
  | n == 15 = "quinze"
  | n == 16 = "dezesseis"
  | n == 17 = "dezessete"
  | n == 18 = "dezoito"
  | n == 19 = "dezenove"
  | (n >= 20 && n<30) = if (n == 20) then "vinte" else "vinte e " ++ numeralParaExtenso(n-20)
  | (n >= 30 && n<40) = if (n == 30) then "trinta" else "trinta e " ++ numeralParaExtenso(n-30)
  | (n >= 40 && n<50) = if (n == 40) then "quarenta" else "quarenta e " ++ numeralParaExtenso(n-40)
  | (n >= 50 && n<60) = if (n == 50) then "cinquenta" else "cinquenta e " ++ numeralParaExtenso(n-50)
  | (n >= 60 && n<70) = if (n == 60) then "sessenta" else "sessenta e " ++ numeralParaExtenso(n-60)
  | (n >= 70 && n<80) = if (n == 70) then "setenta" else "setenta e " ++ numeralParaExtenso(n-70)
  | (n >= 80 && n<90) = if (n == 80) then "oitenta" else "oitenta e " ++ numeralParaExtenso(n-80)
  | (n >= 90 && n<100) = if (n == 90) then "noventa" else "noventa e " ++ numeralParaExtenso(n-90)
  | (n >= 100 && n<200) = if (n == 100) then "cem" else "cento e " ++ numeralParaExtenso(n-100)
  | (n >= 200 && n<300) = if (n == 200) then "duzentos" else "duzentos e " ++ numeralParaExtenso(n-200)
  | (n >= 300 && n<400) = if (n == 300) then "trezentos" else "trezentos e " ++ numeralParaExtenso(n-300)
  | (n >= 400 && n<500) = if (n == 400) then "quatrocentos" else "quatrocentos e " ++ numeralParaExtenso(n-400)
  | (n >= 500 && n<600) = if (n == 500) then "quinhentos" else "quinhentos e " ++ numeralParaExtenso(n-500)
  | (n >= 600 && n<700) = if (n == 600) then "seiscentos" else "seiscentos e " ++ numeralParaExtenso(n-600)
  | (n >= 700 && n<800) = if (n == 700) then "setecentos" else "setecentos e " ++ numeralParaExtenso(n-700)
  | (n >= 800 && n<900) = if (n == 800) then "oitocentos" else "oitocentos e " ++ numeralParaExtenso(n-800)
  | (n >= 900 && n<1000) = if (n == 900) then "novecentos" else "novecentos e " ++ numeralParaExtenso(n-900)
  | (n >= 1000 && n<2000) = if (n == 1000) then "mil" else if (n<1100)then "mil e " ++ numeralParaExtenso(n-1000) 
    else "mil " ++ numeralParaExtenso(n-1000)
  | (n >= 2000 && n<1000000) = if ((mod n 1000)==0) then numeralParaExtenso(div n 1000) ++ " mil" 
    else if (((mod n 100) == 0) ||(n - ((div n 1000)*1000) <= 99)) then numeralParaExtenso (div n 1000) ++ " mil e " ++ numeralParaExtenso(n- ((div n 1000)*1000)) 
    else numeralParaExtenso (div n 1000) ++ " mil, " ++ numeralParaExtenso(n- ((div n 1000)*1000))
  | (n >= 1000000 && n<1000000000) = if (div n (10^6) == 1 && mod n (10^6) == 0) then "um milhao"
    else if (mod n (10^6) == 0) then numeralParaExtenso(div n (10^6)) ++ " milhoes"
    else if ((div n (10^6) == 1) && (n - (10^6)<=100)  ) then "um milhao e " ++ numeralParaExtenso(n-(10^6)) 
    else if ((div n (10^6) == 1) && (n - (10^6)>1000)) then "um milhao, " ++ numeralParaExtenso (n-(10^6)) 
    else if ((div n (10^6) >1) && (n - ((div n (10^6))*(10^6))<=1000)) then numeralParaExtenso(div n (10^6)) ++ " milhoes e " ++ numeralParaExtenso(n- (div n (10^6)) * 10^6)   
    else numeralParaExtenso(div n (10^6)) ++ " milhoes, " ++ numeralParaExtenso(n- (div n (10^6)) * 10^6)
  | (n >= 1000000000 && n<=9999999999) = if (div n (10^9) == 1 && mod n (10^9) == 0) then "um bilhao"
    else if (mod n (10^9) == 0) then numeralParaExtenso(div n (10^9)) ++ " bilhoes"
    else if ((div n (10^9) == 1) && (n - (10^9)<=1000)) then "um bilhao e " ++ numeralParaExtenso(n-(10^9)) 
    else if ((div n (10^9) == 1) && (n - (10^9)>1000)) then "um bilhao, " ++ numeralParaExtenso (n-(10^9)) 
    else if ((div n (10^9) >1) && (n - ((div n (10^9))*(10^9))<=1000)) then numeralParaExtenso(div n (10^9)) ++ " bilhoes e " ++ numeralParaExtenso(n- (div n (10^9)) * 10^9)   
    else numeralParaExtenso(div n (10^9)) ++ " bilhoes, " ++ numeralParaExtenso(n- (div n (10^9)) * 10^9)                                
  | otherwise = "Maior ou igual a dez bilhoes"
