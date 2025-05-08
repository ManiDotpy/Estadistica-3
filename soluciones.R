# Probabilidad de que el sistema no tenga un defecto de tipo 1 (A1^c)
# ========================================
# 1. punto a)
P_A1 <- 0.12  # Probabilidad de que el sistema tenga un defecto de tipo 1

# Cálculo del complemento (probabilidad de que no suceda el defecto tipo 1)
P_A1_complemento <- 1 - P_A1
P_A1_complemento

#1. punto b)
P_A2 <- 0.07
P_A1_union_A2 <- 0.13
# Cálculo de la intersección usando inclusión-exclusión
P_A1_int_A2 <- P_A1 + P_A2 - P_A1_union_A2
P_A1_int_A2

#1. punto c)
#interseccion de los 3 defectos
P_A1_x_A2_x_A3 <- 0.01 

P_A1_x_A2_sin_A3 <- P_A1_int_A2 - P_A1_x_A2_x_A3
P_A1_x_A2_sin_A3

#1. punto d)
# complemento de la triple intersección
P_max_dos_defectos <- 1 - P_A1_x_A2_x_A3

P_max_dos_defectos

# ==========================================
# Permutaciones con repetición (orden + repetición)
# ==========================================

# 2. punto a) 
n_letras <- 26
longitud_dominios <- 2
dominios_2letras <- n_letras^longitud_dominios  
dominios_2letras
# 2. punto a) 
n_caracteres <- 26 + 10
dominios_2caracteres <- n_caracteres^longitud_dominios  
dominios_2caracteres


#2. punto b)
dominios_3letras <- n_letras^3  
dominios_3letras
#2. punto b)
dominios_3caracteres <- n_caracteres^3  
dominios_3caracteres

#2. punto c)
dominios_4letras <- n_letras^4  
dominios_4letras

#2. punto c)
dominios_4caracteres <- n_caracteres^4
dominios_4caracteres

#2. punto d)
no_reclamadas <- 97786
reclamadas <- dominios_4caracteres - no_reclamadas
reclamadas
probabilidad <- reclamadas / dominios_4caracteres
probabilidad

#2.1 punto a)
total_combinaciones <- choose(15, 3)  # 455

focos_75W <- 6
focos_no75W <- 9

combinaciones_2de75 <- choose(focos_75W, 2) 
combinaciones_2de75

combinaciones_1no75 <- choose(focos_no75W, 1)  
combinaciones_1no75

formas_favorables <- combinaciones_2de75 * combinaciones_1no75  
formas_favorables

probabilidad <- formas_favorables / total_combinaciones 
probabilidad

#2.1 punto b)
focos_40W <- 4
focos_60W <- 5

combinaciones_40W <- choose(focos_40W, 3)  
combinaciones_40W
combinaciones_60W <- choose(focos_60W, 3)  
combinaciones_60W
combinaciones_75W <- choose(focos_75W, 3)  
combinaciones_75W

formas_favorables <- combinaciones_40W + combinaciones_60W + combinaciones_75W
formas_favorables
probabilidad2 <- formas_favorables / total_combinaciones 
probabilidad2

#2.1 punto c)
combinaciones_40W <- choose(focos_40W, 1)  
combinaciones_40W
combinaciones_60W <- choose(focos_60W, 1)
combinaciones_60W
combinaciones_75W <- choose(focos_75W, 1)  
combinaciones_75W

formas_favorables <- combinaciones_40W * combinaciones_60W * combinaciones_75W
formas_favorables

probabilidad <- formas_favorables / total_combinaciones  # 0.2637
probabilidad


#2.1 punto d)
no_75W <- 9
total_focos <- 15

probabilidad <- choose(no_75W, 5) / choose(total_focos, 5)  

probabilidad


#3 punto a)
total <- choose(15, 10)          
favorables <- choose(5, 5) * choose(10, 5) 
favorables
prob <- favorables / total
prob

#3 punto b)
favorables <- 3 * choose(5, 5) * choose(10, 5)  
favorables
prob <- favorables / total       
prob
#3 punto c)
total <- choose(15, 6)
total
favorables <- choose(5, 2)^3 
favorables
prob <- favorables / total    
prob

#seccion B
# Probabilidades individuales y conjuntas
P_A1 <- 0.12
P_A2 <- 0.07
P_A3 <- 0.05
P_A1_union_A2 <- 0.13
P_A1_union_A3 <- 0.14
P_A2_union_A3 <- 0.10
P_A1_int_A2_int_A3 <- 0.01

# Cálculo de intersecciones dos a dos
P_A1_int_A2 <- P_A1 + P_A2 - P_A1_union_A2  # 0.06
P_A1_int_A3 <- P_A1 + P_A3 - P_A1_union_A3  # 0.03
P_A2_int_A3 <- P_A2 + P_A3 - P_A2_union_A3  # 0.02

#Punto a 

# P(A2 | A1) = P(A1 ∩ A2) / P(A1)
P_A2_dado_A1 <- P_A1_int_A2 / P_A1  # 0.06 / 0.12
P_A2_dado_A1
#punto b

# P(A2 ∩ A3 | A1) = P(A1 ∩ A2 ∩ A3) / P(A1)
P_tres_dado_A1 <- P_A1_int_A2_int_A3 / P_A1  # 0.01 / 0.12
P_tres_dado_A1

#punto c
# P(Al menos 1 defecto) = P(A1 ∪ A2 ∪ A3) (ya calculado: 0.14)
P_al_menos_1 <- 0.14

# P(Exactamente 1 defecto) = P(A1) + P(A2) + P(A3) - 2[P(A1∩A2) + P(A1∩A3) + P(A2∩A3)] + 3P(A1∩A2∩A3)
P_exactamente_1 <- (P_A1 + P_A2 + P_A3) - 2*(P_A1_int_A2 + P_A1_int_A3 + P_A2_int_A3) + 3*P_A1_int_A2_int_A3  # 0.11

# P(Exactamente 1 | Al menos 1)
P_exactamente_1_dado_al_menos_1 <- P_exactamente_1 / P_al_menos_1  # 0.11 / 0.14

#punto d 
# P(A3^c | A1 ∩ A2) = [P(A1 ∩ A2) - P(A1 ∩ A2 ∩ A3)] / P(A1 ∩ A2)
P_no_A3_dado_A1_A2 <- (P_A1_int_A2 - P_A1_int_A2_int_A3) / P_A1_int_A2  # (0.06 - 0.01) / 0.06

#seccion B.2)
# a.
prob_pequeno <- 0.14 + 0.20  # 0.34
prob_descafeinado <- 0.20 + 0.10 + 0.10  # 0.40
prob_pequeno
prob_descafeinado

# b. 
prob_descafeinado_dado_pequeno <- 0.20 / 0.34  # ≈ 0.5882

# c. 
prob_pequeno_dado_descafeinado <- 0.20 / 0.40  # 0.5




#Problema 3
p_pasa <- 0.6
p_falla <- 1 - p_pasa  # 0.4

# a) P(los tres siguientes pasan)
p_tres_pasan <- p_pasa^3

# b) P(por lo menos uno pasa)
p_al_menos_uno_pasa <- 1 - p_falla^3

# c) P(exactamente uno pasa)
p_exactamente_uno <- choose(3, 1) * p_pasa * p_falla^2

# d) P(a lo sumo uno pasa)
p_max_uno_pasa <- p_falla^3 + choose(3, 1) * p_pasa * p_falla^2

# e) P(todos pasan | ≥1 pasa)
p_todos_dado_al_menos_uno <- p_tres_pasan / p_al_menos_uno_pasa


#Problema 4

p_abre <- 0.95
p_no_abre <- 1 - p_abre  # 0.05

# P(por lo menos una se abre) = 1 - P(ninguna se abre)
p_al_menos_una_abre <- 1 - p_no_abre^5

# P(por lo menos una no se abre) = 1 - P(todas se abren)
p_al_menos_una_no_abre <- 1 - p_abre^5


#Problema 6
# Probabilidad de que un componente funcione
p_trabaja <- 0.9

# a) Subsistema paralelo (1 o 2 funcionan)
p_paralelo <- 1 - (1 - p_trabaja)^2  # 0.99

# b) Subsistema serie (3 y 4 funcionan)
p_serie <- p_trabaja^2  # 0.81

# c) Sistema completo (ambos subsistemas funcionan)
p_sistema <- p_paralelo * p_serie  # 0.8019
