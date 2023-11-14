boxNmustaches = function(sample, d, details = FALSE){
	outliers = c()
	if(details){
		print("->PASO 1: DETERMINACIÓN DEL GRADO DE OUTLIER")
		cat("Se ha introducido un grado de outlier d = ",d,"\n\n")
	}
	cuart1 = quant(sample, 0.25)
	cuart3 = quant(sample, 0.75)
	if (details){
		print("->PASO 2: CÁLCULO DE LOS CUARTILES 1 Y 3")
		cat("El cuartil 1 es ",cuart1, " y el cuartil 3 es ", cuart3,"\n\n")
	}
	lim_inf = cuart1 - (d*(cuart3-cuart1))
	lim_sup = cuart3 + (d*(cuart3-cuart1))
	if (details){
		print("->PASO 3: CÁLCULO DE LOS LÍMITES DEL INTERVALO PARA VALORES ATÍPICOS")
		cat("Fórmula -> (Q1 - d*(Q3-Q1), Q3 + d*(Q3-Q1))\n")
		cat("El intervalo para los valores atípicos es: (",lim_inf,", ",lim_sup,")\n\n")
	}
	if(details){
		print("->PASO 4: IDENTIFICACIÓN DE OUTLIERS")
		cat("Los outliers serán aquellos valores que no están en el intervalo anterior\n")
	}
	for (i in 1:len(sample)){
		if(sample[i] < lim_inf || sample[i] > lim_sup){
			outliers = append(outliers, i)
			if(details){
				cat("El punto ",i," con valor ",sample[i]," es un outlier\n")
			}
		}
	}
	outliers
}

statistic_outliers = function(sample, d, details = FALSE){
	outliers = c()
	if(details){
		print("->PASO 1: DETERMINACIÓN DEL GRADO DE OUTLIER")
		cat("Se ha introducido un grado de outlier d = ",d,"\n\n")
	}
	mean_sample = mean(sample)
	if (details){
		print("->PASO 2: CÁLCULO DE LA MEDIA ARITMÉTICA")
		cat("La media aritmética es ", mean_sample, "\n\n")
	}
	dev_sample = standard_dev(sample)
	if (details){
		print("->PASO 3: CÁLCULO DE LA DESVIACIÓN TÍPICA")
		cat("La desviación típica es ", dev_sample, "\n\n")
	}
	lim_inf = mean_sample - (d*dev_sample)
	lim_sup = mean_sample + (d*dev_sample)
	if (details){
		print("->PASO 4: CÁLCULO DE LOS LÍMITES DEL INTERVALO PARA VALORES ATÍPICOS")
		cat("Fórmula -> (Media - d*DesviaciónTípica, Media + d*DesviaciónTípica)\n")
		cat("El intervalo para los valores atípicos es: (",lim_inf,", ",lim_sup,")\n\n")
	}
	if(details){
		print("->PASO 5: IDENTIFICACIÓN DE OUTLIERS")
		cat("Los outliers serán aquellos valores que no están en el intervalo anterior\n")
	}
	for (i in 1:len(sample)){
		if(sample[i] < lim_inf || sample[i] > lim_sup){
			outliers = append(outliers, i)
			if(details){
				cat("El punto ",i," con valor ",sample[i]," es un outlier\n")
			}
		}
	}

	outliers
}

a = boxNmustaches(c(3,3.5,4.7,5.2,6.2,7.1,14),1.5,TRUE)
b = statistic_outliers(c(2,12,4.1,4.9,6.1,5.2,5.3),2,TRUE)