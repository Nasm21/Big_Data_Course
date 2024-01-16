#!/usr/bin/env python
# coding: utf-8

# # Redes Neuronales

# In[1]:


get_ipython().system('pip install tensorflow')
'''Es un marco de aprendizaje automático de código abierto que se utiliza 
para construir y entrenar modelos de aprendizaje'''


# In[2]:


'''Estas bibliotecas son muy utilizadas en el ámbito del aprendizaje automático y la inteligencia artificial. 
TensorFlow proporciona herramientas para construir y entrenar modelos de aprendizaje automático, 
mientras que NumPy ofrece funciones y operaciones eficientes para trabajar con matrices y realizar 
cálculos numéricos. '''

import tensorflow as tf 
import numpy as np


# In[3]:


celsius = np.array([-40, -10, 0, 8, 15, 22, 38], dtype=float) #entrada
fahrenheit = np.array([-40, 14, 32, 46, 59, 72, 100], dtype=float) #Salida

type(celsius)


# In[15]:


# Crear una capa densa (fully connected)
# - 'units=1': La capa tiene un solo nodo, común en modelos de regresión lineal.
# - 'input_shape=[1]': La capa espera entradas unidimensionales.
capa = tf.keras.layers.Dense(units=1, input_shape=[1]) 

# Crear un modelo secuencial
# - Se pasa la capa creada anteriormente como único elemento en la lista.
modelo = tf.keras.Sequential([capa])


# In[16]:


# Configurar el modelo para el entrenamiento
#Optimizador= Adam es un algoritmo de optimización que ajusta automáticamente la tasa de aprendizaje 
#durante el entrenamiento. La tasa de aprendizaje está configurada en 0.1
#Loss= Se establece la función de pérdida como el error cuadrático medio (residuales).
modelo.compile(
    optimizer=tf.keras.optimizers.Adam(0.1),
    loss='mean_squared_error'
)


# In[17]:


# Entrenar el modelo utilizando los datos de entrada 'celsius' y salida 'fahrenheit'
# - 'epochs=380': Número de veces que el modelo pasará por todos los datos de entrenamiento 
# durante el entrenamiento.
# - 'verbose=False': Desactiva la visualización de información detallada durante el entrenamiento.
print("Comenzando entrenamiento...")
historial = modelo.fit(celsius, fahrenheit, epochs=400, verbose=False)
print("Modelo entrenado!")


# In[18]:


import matplotlib.pyplot as plt
plt.xlabel("# Epoca")
plt.ylabel("Magnitud de pérdida")
# Graficar el historial de pérdida durante el entrenamiento
plt.plot(historial.history["loss"])


# In[19]:


print("Hagamos una predicción!")
resultado = modelo.predict([46])
print("El resultado es " + str(resultado) + " Fahrenheit!")


# In[20]:


print("Variables internas del modelo")
print("            peso     " + "                         sesgo")

print(capa.get_weights())


# In[32]:


oculta1 = tf.keras.layers.Dense(units=3, input_shape=[1])
oculta2 = tf.keras.layers.Dense(units=2)
oculta3 = tf.keras.layers.Dense(units=2)
modelo = tf.keras.Sequential([oculta1, oculta2, oculta3])


# In[33]:


modelo.compile(
    optimizer=tf.keras.optimizers.Adam(0.1),
    loss='mean_squared_error'
)


# In[34]:


print("Comenzando entrenamiento...")
historial = modelo.fit(celsius, fahrenheit, epochs=80, verbose=False)
print("Modelo entrenado!")


# In[15]:


import matplotlib.pyplot as plt
plt.xlabel("# Epoca")
plt.ylabel("Magnitud de pérdida")
plt.plot(historial.history["loss"])


# In[35]:


print("Hagamos una predicción!")
# Realizar una predicción utilizando el modelo entrenado
resultado = modelo.predict([46])
print("El resultado es " + str(resultado) + " Fahrenheit!")


# In[17]:


#Guardar el modelo en formato h5
modelo.save('celsius_fahrenheit.h5')


# In[31]:


# Crear el modelo de red neuronal con más neuronas

import numpy as np
import tensorflow as tf
from tensorflow import keras

# Datos de entrenamiento
celsius = np.array([-40, -10, 0, 8, 15, 22, 38], dtype=float)
fahrenheit = np.array([-40, 14, 32, 46, 59, 72, 100], dtype=float)


# Crear un modelo secuencial
model = keras.Sequential([
    # Primera capa densa con 1000 neuronas y especificando la forma de entrada como [1]
    keras.layers.Dense(units=1000, input_shape=[1]),  # Capa densa con N neuronas.
    # Segunda capa densa con 5 neuronas de salida
    keras.layers.Dense(units=1)  # Capa con N neuronas de salida.
])

# Compilar el modelo
model.compile(optimizer='adam', loss='mean_squared_error')

# Entrenar el modelo
model.fit(celsius, fahrenheit, epochs=1000, verbose=0)

# Realizar una predicción
celsius_to_predict = 45.8
predicted_fahrenheit = model.predict([celsius_to_predict])
print(f"{celsius_to_predict} grados Celsius son aproximadamente {predicted_fahrenheit} grados Fahrenheit.")


# In[28]:


# Evaluar el modelo
from sklearn.metrics import mean_squared_error
import math

# Realizar predicciones para los datos de entrenamiento
predicciones_entrenamiento = model.predict(celsius)

# Calcular el error cuadrático medio (MSE)
mse = mean_squared_error(fahrenheit, predicciones_entrenamiento)

# Calcular la raíz cuadrada del error cuadrático medio (RMSE)
rmse = math.sqrt(mse)

# Imprimir las métricas
print(f"Error Cuadrático Medio (MSE): {mse}")
print(f"Raíz Cuadrada del Error Cuadrático Medio (RMSE): {rmse}")


# Puedes ajustar y utilizar otras métricas según tus necesidades específicas. Además, ten en cuenta que estas métricas son específicas para los datos de entrenamiento. Para una evaluación más completa, podrías dividir tus datos en conjuntos de entrenamiento y prueba y calcular las métricas en el conjunto de prueba para evaluar la generalización del modelo a datos no vistos.

# In[ ]:




