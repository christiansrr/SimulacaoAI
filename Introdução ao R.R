#------------------------------
# Operacoes aritmeticas
#------------------------------

2+3

10-2

5*4

20/5

#------------------------------
# Variaveis e atribuicoes
#------------------------------

# Criando uma variavle x e atribuindo a atribuido o valor 3
x = 3

# Exibindo o valor da variavel
x

# Criando um variavle y e atribuindo a atribuido o valor 2
y = 2

# Operacoes aritmeticas com variaveis
x + y

#------------------------------
# Vetores
#------------------------------

v1 = c(2,4,6,8)
v1
v1[3]
length(v1)
max(v1)
min(v1)
sqrt(v1)

v2 = c(2,2,2,2)
v1*v2

#------------------------------
# Matrizes
#------------------------------

M1 = matrix(c(1,2,4,6,
              2,3,8,9,
              2,5,7,3), ncol = 4, byrow = T)

M2 = matrix(c(2,2,2,2,
              2,2,2,2,
              2,2,2,2), ncol = 4, byrow = T)

M1 * M2

#------------------------------
# Listas
#------------------------------

l1 = list(x,y,v1,v2,M1,M2)
l1
l1[1]
l1[4]

#------------------------------
# Funcoes
#------------------------------

f1 = function(a,b) {
  z = (a^3) + (b^2) + 10
  return(z)
}

f1(2,3)
