import random
from statistics import mean


def zgeneriraj_poln_graf(n,m=0):
    sosedi = {};
    for i in range(1,n+1):
        sosedi[i+m] = [];
    for i in range(1,n+1):
        for j in range(1,n+1):
            if j!=i:
                sosedi[i+m].append(j+m);
    return sosedi


def zgeneriraj_pot(n,m=0):
    sosedi = {};
    for i in range(1,n+1):
        sosedi[i+m] = [];
        if n == 1:
            return sosedi
    for i in range(1,n+1):
        if i == 1:
            sosedi[i+m].append(2+m);
        elif i==n:
            sosedi[i+m].append(n-1+m);
        else:
            sosedi[i+m].append(i+m-1);
            sosedi[i+m].append(i+m+1);
    return sosedi
        
def zgeneriraj_liziko(m,n):
    poln = zgeneriraj_poln_graf(m);
    pot = zgeneriraj_pot(n,m);
    poln.update(pot);
    sosedi=poln;
    sosedi[m].append(m+1);
    sosedi[m+1].append(m);
    return sosedi
    
def zgeneriraj_pot_s_kliko_na_obeh_koncih(m,n):
    lizika = zgeneriraj_liziko(m,n);
    klika = zgeneriraj_poln_graf(m,m+n);
    klika.update(lizika);
    sosedi=klika;
    sosedi[m+n].append(m+n+1);
    sosedi[m+n+1].append(m+n);
    return sosedi
    
def zgeneriraj_dve_kliki_na_koncu_poti(m,n):
    lizika = zgeneriraj_liziko(m,n);
    klika = zgeneriraj_poln_graf(m-1,m+n);
    klika.update(lizika);
    sosedi=klika;
    for i in range(m+n+1,m+n+m):
        sosedi[m].append(i);
        sosedi[i].append(m);
    return sosedi

def zgeneriraj_sonce(m,n):
    poln = zgeneriraj_poln_graf(m);
    sosedi = poln;
    for i in range(1,m+1):
        pot = zgeneriraj_pot(n,m+(i-1)*n);
        sosedi.update(pot);
        sosedi[i].append(m+(i-1)*n+1);
        sosedi[m+(i-1)*n+1].append(i);
    return sosedi

def sprehod_po_grafu(zacetno, graf):
    koraki = 0;
    vozlisce = zacetno;
    obiskana = [];
    obiskana.append(vozlisce);
    while len(obiskana) != len(graf):
        koraki += 1;
        vozlisce = random.choice(graf[vozlisce]);
        if vozlisce not in obiskana:
            obiskana.append(vozlisce);
    return koraki
        
def sprehod_do_vozlisca(zacetno, koncno, graf):
    koraki = 0;
    vozlisce = zacetno;
    while vozlisce != koncno:
        koraki += 1;
        vozlisce = random.choice(graf[vozlisce]);
    return koraki

def sprehod_do_vrnitve(zacetno,graf):
    koraki = 1;
    vozlisce = zacetno;
    vozlisce = random.choice(graf[vozlisce]);
    while vozlisce != zacetno:
        koraki += 1;
        vozlisce = random.choice(graf[vozlisce]);
    return koraki
        
def cover_time(zacetno, graf, ponovitve):
    cas = [];
    for i in range(1,ponovitve+1):
        sprehod = sprehod_po_grafu(zacetno, graf);
        cas.append(sprehod);
    return mean(cas)

def hitting_time(zacetno, koncno, graf, ponovitve):
    cas = [];
    for i in range(1,ponovitve+1):
        sprehod = sprehod_do_vozlisca(zacetno, koncno, graf);
        cas.append(sprehod);
    return mean(cas)

def return_time(zacetno, graf, ponovitve):
    cas = [];
    for i in range(1,ponovitve+1):
        sprehod = sprehod_do_vrnitve(zacetno, graf);
        cas.append(sprehod);
    return mean(cas)
