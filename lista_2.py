# -*- coding: cp1252 -*-
#Monitoria 2018.2
#Lista 2 - funções

#questao 1
def media(a,b):
    return (a+b)/2.0

def med_4(x1,x2,x3,x4):
    return media(media(x1,x2),media(x3,x4))

def bombons(dinheiro, preco):
    return dinheiro//preco, dinheiro%preco

import math

#questao 2
def hipotenusa(cat1, cat2):
    return math.hypot(cat1,cat2)

def distancia(x1,x2,y1,y2):
    return math.hypot((x2-x1)**2, (y2-y1)**2)

def per_triangulo(cat1,cat2):
    return cat1+ cat2+ hipotenusa(cat1,cat2)

def soma_angulos(angulo):
    return math.sin(angulo)**2 + math.cos(angulo)**2

#questao 3
def circulo(raio):
    return 2*math.pi*raio

#questao 4
def maratona(raio, dist):
    return dist/circulo(raio)

#questao 5
def delta(a,b,c):
    return b**2 - 4*a*c

def raizes(a,b,c):
    d = delta(a,b,c)
    r1 = (-b+ d)/(2.0*a)
    r2= (b+d)/(2.0*a)
    return r1,r2

#questao 6
def area_circulo(raio, ang=360):
    return (ang/360.0)*math.pi*raio**2

#questao 8
def bolo(a,b,c):
    farinha = a/2
    ovos= b/3
    leite = c/5
    return min(farinha, ovos, leite)

