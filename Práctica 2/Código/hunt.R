
source("aux_functions.R")
get_frec = function(sample, col, criteria, elements, values) {
	n = 0
	frecs = setNames(data.frame(matrix(rep(0, times = length(values)), nrow = 1)), values)

	for (i in 1:len(sample[, col])) {
		if (sample[i, col] %in% elements) {

			frecs[1, sample[i, criteria]] = frecs[1, sample[i, criteria]] + 1
			n = n + 1
		}
	}
	frecs / n
}

entropy = function(frec) {
	acum = 0
	for (i in 1:len(frec)) {
		f = frec[1, i]
		if (f != 0 && f != 1) {
			acum = acum - f * log(f, 2)
		}
	}
	acum
}

error = function(frec) {
	1 - max(frec)
}

gini = function(frec) {
	acum = 0

	for (i in 1:len(frec)) {
		f = frec[1, i]
		acum = acum + f^2
	}
	1 - acum
}

get_impurity = function(sample, col, elements, criteria, equivalence_clases, measure) {

	frec = get_frec(sample, col, criteria, elements, equivalence_clases)
	switch(measure, "entropy" = entropy(frec), "error" = error(frec), "gini" = gini(frec))
}

get_gain = function(sample, left, right, criteria, col, measure) {
	n = len(sample)
	equivalence_clases = get_elements(sample, criteria)

	impurity_father = get_impurity(sample, col, union(left, right), criteria, equivalence_clases, measure)
	impurity_left = get_impurity(sample, col, left, criteria, equivalence_clases, measure)
	impurity_right = get_impurity(sample, col, right, criteria, equivalence_clases, measure)

	impurity_father - (len(left) / n * impurity_left + len(right) / n * impurity_right)
}

get_elements  = function(sample, col) {
	elements = c()
	for (element in sample[, col]) {
		if (!(element %in% elements)) {
			elements = c(elements, element)
		}
	}
	elements
}

create_classification = function(sample, col, right_element, criteria, measure) {

	left = c()

	for (i in 1:len(sample[, col])) {
		element = sample[i, col]

		if (!(element %in% left) && sample[i, criteria] != right_element) {
			left = unlist(append(left, element))
		}
	}
	right = dif(get_elements(sample, col), left)

	if(len(right) > 0 && len(left) > 0) {
		gain = get_gain(sample, left, right, criteria, col, measure)
		
	} else {
		gain = 0
	}

	data.frame(parent=col, right_element, left=I(list(left)), right=I(list(right)), gain)
}

df_2_tree = function(df, final, criteria) {
  
  last_tree = Node$new(df[nrow(df), "parent"])
  last_tree$gain = round(df[nrow(df), "gain"], 3)
  SetNodeStyle(last_tree, label = paste(last_tree$name, "\n ΔI =", last_tree$gain))
  
  new_left_leaf = Node$new(paste(get_elements(final, criteria)))
  new_left_leaf$label = final[nrow(final), df[nrow(df), "parent"]]
  new_left_leaf$gain = ""
  last_tree$AddChildNode(new_left_leaf)
  
  new_right_leaf = Node$new(df[nrow(df), "right_element"])
  new_right_leaf$label = df[nrow(df), "right"]
  new_right_leaf$gain = ""
  last_tree$AddChildNode(new_right_leaf)
  
  SetEdgeStyle(new_left_leaf, label = new_left_leaf$label, fontsize = 11)
  SetEdgeStyle(new_right_leaf, label = new_right_leaf$label, fontsize = 11)
  
  SetNodeStyle(new_left_leaf, label = paste(new_left_leaf$name))
  SetNodeStyle(new_right_leaf, label = paste(new_right_leaf$name))
  
  for (row_index in (nrow(df)-1):1) {
    last_tree$label = df[row_index, "left"]
    new_tree = Node$new(df[row_index, "parent"])
    new_tree$AddChildNode(last_tree)
    
    right_node = Node$new(df[row_index, "right_element"])
    right_node$label = df[row_index, "right"]
    right_node$gain = ""
    new_tree$AddChildNode(right_node)
    
    SetEdgeStyle(right_node, label = right_node$label, fontsize = 11)
    SetEdgeStyle(last_tree, label = last_tree$label, fontsize = 11)
    
    last_tree = new_tree
    
    last_tree$gain = round(df[row_index, "gain"], 3)
    SetNodeStyle(last_tree, label = paste(last_tree$name, "\n ΔI =", last_tree$gain))
    SetNodeStyle(right_node, label = paste(right_node$name))
  }

  
  return(last_tree)
}

hunt = function(sample, classes, criteria, measure, details = FALSE) {

	final_clasification = data.frame()
	best_gain = -1
	best_clasification = NULL

	while (nrow(sample) > 0 && best_gain != 0) {
		best_gain = -1
		for (col in classes) {
			for (element in get_elements(sample, criteria)) {

				clasification = create_classification(sample, col, element, criteria, measure)

				actual_gain = clasification$gain

				if (actual_gain > best_gain) {
					best_gain = actual_gain
					best_clasification = clasification
				}
			}
		}

		if (best_gain != 0) {
			sample = subset(sample, sample[, best_clasification$parent] %in% unlist(best_clasification$left))
			final_clasification = rbind(final_clasification, best_clasification)
			
			if (details) {
			  cat("\nIteración", nrow(final_clasification))
			  cat("\n============\n")
			  cat("Se escoge el nodo", best_clasification$parent, "con una ganacia de", best_gain)
			  cat("\nLos sucesos que tengan el/los valor/es", paste(best_clasification$right), "en ese nodo se clasifican como", best_clasification$right_element)
			  cat("\nLa nueva muestra es:\n")
			  print(sample)
			  cat("\n")
			}
		}
	}
	
	tree = df_2_tree(final_clasification, sample, criteria)
	
	cat("\nÁrbol de decisión\n=================\n")
	print(tree, "label", "gain")
	
	tree
}

(sample = read.xlsx("../Memoria/data/vehiculos.xlsx"))

tree = hunt(sample, c("tCarnet", "nRuedas", "nPasajeros"), "tVehículo", "error", TRUE)
plot(tree)