# -*- coding: utf-8 -*-
"""
Created on Tue Mar 23 17:10:48 2021

@author: Daniel Sanchez
"""
# Analizamos la base de corrupción de países

import pandas as pd # para manejo de bases
import numpy as np # para valores aleatorios
import matplotlib.pyplot as plt # para gráficos
import seaborn as sns # para correlogramas

# porsiacaso un vector con valores aleatoriosx=np.random.randn(0,1) # valores aleatorios de 0 a 1

# el punto abrevia la dirección de donde esta situado el script
# si un archivo no está en la carpeta donde está el script hay que especificar el path
# necesita siempre el slash normal, no sirve con backslash, igual que R

# Primero importamos la base en csv

df=pd.read_csv('./files/cor1.csv')
print(df.head())

# ahora sacamos las filas con missing values
df_nac=df.dropna()
# no es tan bueno para cálculos porque quedan solo los países que tienen TODOS los valores

# creemos la variable del logaritmo de gdp y de la gente

df['lgdppc']=np.log(df['gdp_pc'])
df['lpop']=np.log(df['pop'])

# Vamos con estadísticos descriptivos
summary_df=df.describe()
print(summary_df)

# Exportamos los estadísticos descriptivos
summary_df.to_excel('descriptive stats.xlsx')

# Vamos a correr correlaciones para las variables, matriz de correlacion de cci con algunas
df1=df[['cci','lgdppc','agedem','efw','lpop']]

# necesitamos quitar los infinitos que surgieron del logaritmo
df1.replace(-np.inf,np.nan, inplace=True) # primero convertimos los infinitos
df1.dropna(inplace=True) # luego saco los nas

# ahora si el correlograma
sns.pairplot(df1)
plt.show()


