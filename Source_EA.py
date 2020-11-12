#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 17 23:15:50 2020

@author: Fred Valdez Ameneyro

My Evolutionary Algorithm for Decision Trees
Features:
Evolution seeded with the output of existing methods
Evolve towards different objectives
Can output multiple trees with particular advantages when there are conflicting objectives
Existing solutions can be evolved further
test
"""
import numpy as np
import random as rd
import operator as op
import math
from inspect import signature
import pandas as pd
import copy
import statistics as st
from collections import defaultdict
import pickle

#text_to_operator = {"<":op.lt,">=":op.ge,"<=":op.le,">":op.gt,"==":op.eq,"!=":op.ne}
#operator_to_text = {op.lt:"<",op.ge:">=",op.le:"<=",op.gt:">",op.eq:"==",op.ne:"!="}
#inverse_operators = {op.lt:op.ge,op.le:op.gt,op.gt:op.le,op.ge:op.lt,op.eq:op.ne}

class Objective:
	def __init__(self, objective_name, index, to_max = True, best = None, worst = None):
		self.objective_name = objective_name
		self.index = index
		self.to_max = to_max
		self.best = best
		self.worst = worst
		print("created objective: ", objective_name)

	def update_best(value):
		self.best = value

	def update_worst(value):
		self.worst = value

class Individual: #missing __lt__ and __eq__
	def __init__(
			self,
			generation_of_creation,
			genotype,
			n_objectives = 0,
			phenotype = None):

		self.generation_of_creation = generation_of_creation
		self.genotype = genotype
		self.phenotype = phenotype
		
		self.evaluated_on_static_objectives = False
		self.rank = None                                 #used in NSGA-II
		self.dominated_solutions = []                    #used in NSGA-II
		self.domination_counter = None                   #used in NSGA-II
		self.crowding_distance = None                    #used in NSGA-II
		self.objective_values = [None for _ in range(n_objectives)]

	def change_n_objectives(self, new_n_objectives):
		self.evaluated_on_static_objectives = False
		self.objective_values = [None for _ in range(new_n_objectives)]

	def __str__(self):
		return "Ind. Genotype:" + str(self.genotype) + ", obj values:" + str(self.objective_values)

class Attribute:
	def __init__(self,index,name,type = None,crucial_values = [],all_values = [],available_values = []):
		"""
		s
		"""
		self.index = index
		self.name = name
		self.type = type
		self.all_values = all_values
		self.crucial_values = crucial_values
		self.available_values = available_values

	def add_crucial_value(self, value):
		"""
		crucial values are the relevant values that are used to split the data in the trees
		returns: the index in the attribute of the newly added value
		"""
		if value in self.crucial_values:
			index = self.crucial_values.index(value)
			return index
		else:
			index = len(self.crucial_values)
			self.crucial_values = self.crucial_values + [value]
			print("Added crucial value ", str(value), " to attribute ", self.name, ". All values:", str(self.crucial_values))
			return index

	def __str__(self):
		return "Attribute " + self.name + ",idx:" + str(self.index) + ",n_crucial_values:" + str(len(self.crucial_values))

class DT_Node:
	def __init__(self, parent = None):
		"""
		children are sorted: the first one is the true, the second one is the false
		"""
		#Initial values
		self.visits_count = 0
		self.gini_index = None
		self.parent=parent
		#All the following variables need to be copied by the copy() function
		self.updated=False #only used when parsing trees from R
		self.children = []
		self.children_names = []
		self.output_label = None
		self.attribute = None
		self.operator = None
		self.operator_name = None
		self.comparable_value = None
		self.comparable_value_index = None
		self.initial_output_label_count = {}
		self.output_label_count = {}

	def update(self,attribute,comparable_value_index,comparable_value,operator,operator_name, output_labels): #only used when parsing trees from R
		self.updated = True
		self.attribute = attribute
		self.comparable_value = comparable_value #float(self.attribute.crucial_values[comparable_value_index]) #all are floats rn
		self.comparable_value_index = comparable_value_index
		self.operator = operator
		self.operator_name = operator_name
		self.initial_output_label_count = {label:0 for label in output_labels}
		self.output_label_count = {label:0 for label in output_labels}

	def reset_numbers(self):
		self.visits_count = 0
		self.output_label_count = {key:value for key,value in self.initial_output_label_count.items()}

	def reset_tree_numbers(self):
		for node in self.get_subtree_nodes():
			node.reset_numbers()

	def add_child(self,name,child,index=None):
		child.parent = self
		if index is None:
			self.children = self.children + [child]
			self.children_names = self.children_names + [name]
		else:
			self.children[index] = child
			self.children_names[index] = name
		#print("Added child named ", name , ",new child count", str(len(self.children)))
		return child

	def add_new_child(self, name):
		empty_node = DT_Node()
		child = self.add_child(name=name,child=empty_node, index = None)
		return child

	def replace_child(self, new_child, old_child):
		index = self.children.index(old_child)
		self.add_child(name = self.children_names[index], child = new_child, index = index)

	def get_gini_index(self):
		if self.visits_count > 0:
			gini = 1 - sum([(label_count/self.visits_count)**2 for label_count in self.output_label_count.values()])
			return gini
		else:
			return None

	def evaluate(self,data_row, label):
		self.visits_count = self.visits_count + 1
		if self.output_label is None:
			#self.output_label_count = [self.output_label_count]
			self.output_label_count[label] = self.output_label_count[label] + 1
			try:
				comparison = self.operator(data_row.iloc[self.attribute.index],self.comparable_value)
			except:
				print("Error in evaluation")
				return None

			if comparison == True:
				#print("Going to child", self.children_names[0])
				return self.children[0].evaluate(data_row, label) #can be improved
			else:
				#print("Going to child", self.children_names[1])
				return self.children[1].evaluate(data_row, label)

		else:
			#print("Output:",self.output_label)
			return self.output_label

	def is_terminal(self):
		return self.children == []

	def is_root(self):
		return self.parent is None

	def get_subtree_nodes(self, include_self = True, include_terminals = True): #can be optimised
		"""
		Returns a list with all the nodes of the subtree with this node as the root node, including himself
		"""
		nodes = [self]
		i = 0
		while i < len(nodes):
			for child in nodes[i].children:
				if include_terminals:
					nodes.append(child)
				else:
					if not child.is_terminal():
						nodes.append(child)
			i += 1
		if include_self:
			return nodes
		else:
			return nodes[1:]

	def get_connections(self, include_terminals = True): #can be optimised
		"""
		Returns a list with all the nodes of the subtree with this node as the root node, including himself
		"""
		nodes = [self]
		c_from = []
		c_to = []
		c_color = []
		i = 0
		while i < len(nodes):
			for child_index, child in enumerate(nodes[i].children):
				if (not child.is_terminal() and not include_terminals) or include_terminals:
					c_from.append(i)
					c_to.append(len(nodes))
					if child_index == 0:
						c_color.append("blue")
					else:
						c_color.append("red")
					nodes.append(child)
			i += 1
		return c_to, c_from, c_color

	def get_max_depth(self, depth = 0):
		"""
		Returns the max depth of this tree as an int
		"""
		new_depth = depth + 1
		if self.is_terminal():
			return new_depth
		else:
			return max([child.my_depth(new_depth) for child in self.children])

	def node_already_in_branch(self, node=None): #missing self.__eq__()
		if self.parent is None:
			return False
		else:
			if self == node:
				return True
			else:
				return parent.node_already_in_branch(node=node)

	def copy(self, parent=None): #missing test: evaluate the copy
		"""
		Returns an unrelated new item with the same characteristics
		"""
		the_copy = DT_Node(parent = parent)
		the_copy.updated=self.updated
		the_copy.children_names = [n for n in self.children_names]
		the_copy.output_label = self.output_label
		the_copy.attribute = self.attribute
		the_copy.operator = self.operator
		the_copy.operator_name = self.operator_name
		the_copy.comparable_value = self.comparable_value
		the_copy.comparable_value_index = self.comparable_value_index
		the_copy.initial_output_label_count = {k:v for k,v in self.initial_output_label_count.items()}
		the_copy.output_label_count = {k:v for k,v in self.output_label_count.items()}

		for child in self.children:
			copy_child = child.copy(parent=the_copy)
			the_copy.children.append(copy_child)

		return the_copy

	def __str__(self):
			if self.is_terminal():
				return "Class: " + str(self.output_label) + "\nVisits:" + str(self.visits_count)
			else:
				gini = self.get_gini_index()
				if gini is None:
					gini_string = "N/A"
				else:
					gini_string = "%.2f"%gini
				return self.attribute.name +"\n"+ self.operator_name + str(self.comparable_value) + "\nVisits:" + str(self.visits_count) + "\nGini:" + gini_string

	#def __eq__(self, other):

class DecisionTree_EA: #oblique, binary trees
	def __init__(self, tournament_size = 3, crossover_rate = 0.5, mutation_rate = 0.4, elitism_rate = 0.1, hall_of_fame_size = 3):

		self.output_labels = []
		self.current_generation = 0
		self.unique_output_labels = []
		self.attributes = {}                                                                      #{attribute index : attribute class object}
		self.operators = []
		self.objectives = {}                                                                      #{objective index : objective class object}
		self.n_objectives = 0
		self.generation = 0
		self.population = []
		self.parsing_operators = []
		self.operators_db = {"<":op.lt,">=":op.ge,"<=":op.le,">":op.gt,"==":op.eq,"!=":op.ne}
		self.inverse_operators = {op.lt:op.ge,op.le:op.gt,op.gt:op.le,op.ge:op.lt,op.eq:op.ne}
		self.tournament_size = int(tournament_size)
		print("Called class DecisionTree_EA")
		self.archive_population = []
		self.hall_of_fame_size = int(hall_of_fame_size)
		self.hall_of_fame = []
		self.crossover_rate = crossover_rate
		self.mutation_rate = mutation_rate
		self.elitism_rate = elitism_rate
		self.crossovers = 0
		self.mutations = 0
		self.elites = 0
		self.fronts = defaultdict(lambda:[])                   #used in NSGA-II

	def add_operator(self,operator_name): #operators with compatibility to certain attributes?
		if operator_name not in self.operators:
			self.operators.append(operator_name)
			print("Added new operator:",operator_name)

	def add_objective(self, objective_name, to_max = True, best = None, worst = None):
		current_objective_names = [obj.objective_name for obj in self.objectives.values()]
		do_add = True
		if len(current_objective_names) > 0:
			if objective_name in current_objective_names:
				do_add = False
				print("Objective ", objective_name, " is already being considered")

		if do_add:
			self.objectives[self.n_objectives] = Objective(objective_name = objective_name,
															index = self.n_objectives,
															to_max = to_max,
															best = best,
															worst = worst)
			#self.n_objectives = self.n_objectives + 1
			self.n_objectives = len(self.objectives.items())

			if len(self.population) > 0:
				for ind in self.population:
					ind.change_n_objectives(new_n_objectives = self.n_objectives)
			print("Added objective ", objective_name)

	def add_attribute(self, name, index):
		self.attributes[name] = Attribute(index=index, name=name)
		print("added attribute ",name)

	def remove_objective(self, objective_name):
		if len(self.objectives.items()) > 0:
			"""
			The objective object with name=objective_name is removed from self.objectives
			"""
			#Try to remove the objective
			self.objectives = {key:val for key, val in self.objectives.items() if val.objective_name != objective_name}
			
			#Ensure that the keys of the objectives start from 0: 
			counter = 0
			temp_objectives = {}
			for old_key, objective in self.objectives.items():
				temp_objectives[counter] = objective
				counter = counter + 1
			self.objectives = temp_objectives
			
			#Update the individuals if the number of objectives changed
			if self.n_objectives != len(self.objectives.items()):
				self.n_objectives = len(self.objectives.items())
				for individual in self.population:
					individual.change_n_objectives(self.n_objectives)
				print("Removed objective ", objective_name)
			else:
				print("The objective ", objective_name," is not being considered")
		else:
			print("There are no objectives")

	def _one_point_crossover(self, individual1, individual2):
		"""
		A random node in each tree is selected. The corresponding subtrees are swapped
		"""
		tree1 = individual1.genotype
		tree2 = individual2.genotype

		copy1 = tree1.copy()
		copy2 = tree2.copy()

		#There is a 50% chance to select a terminal to do the swap
		include_terminals = rd.choice([True, False])
		nodes1 = copy1.get_subtree_nodes(include_self = False, include_terminals = False)
		nodes2 = copy2.get_subtree_nodes(include_self = False, include_terminals = False)

		node1 = rd.choice(nodes1)
		node2 = rd.choice(nodes2)

		temp1 = node1.copy()
		temp2 = node2.copy()
		node1.parent.replace_child(new_child=temp2, old_child=node1)
		node2.parent.replace_child(new_child=temp1, old_child=node2)

		new_individual1 = Individual(generation_of_creation = self.generation, genotype = copy1, n_objectives = self.n_objectives)
		new_individual2 = Individual(generation_of_creation = self.generation, genotype = copy2, n_objectives = self.n_objectives)

		return [new_individual1, new_individual2]

	def _single_point_mutation(self, individual):#missing, new nodes have no names for their children
		"""
		Changes the attribute, operator and value of the randomly selected node in the tree.
		Terminals cannot be selected
		"""
		tree = individual.genotype
		tree_copy = tree.copy()
		nodes = tree_copy.get_subtree_nodes(include_self = False, include_terminals = False)
		unlucky_node = rd.choice(nodes)
		new_node = self.generate_random_node()
		unlucky_node.parent.replace_child(new_child=new_node, old_child=unlucky_node)
		for child in unlucky_node.children:
			new_node.add_child(name="Noname",child=child)
		new_individual = Individual(generation_of_creation = self.generation, genotype = tree_copy, n_objectives = self.n_objectives)
		return new_individual
		
	def _subtree_mutation(self, individual):
		"""
		Swaps a randomly selected subtree in the individual with a randomly generated tree.
		"""
		tree = individual.genotype
		tree_copy = tree.copy()
		nodes = tree_copy.get_subtree_nodes(include_self = False, include_terminals = True)
		#include_terminals = rd.choice([True, False])
		unlucky_node = rd.choice(nodes)
		subtree_min_depth = 1
		#subtree_max_depth = rd.randint(subtree_min_depth, len(self.attributes))
		subtree_max_depth = rd.randint(subtree_min_depth, 3)
		new_subtree = self.generate_random_tree(max_depth=subtree_max_depth, min_depth = subtree_min_depth, method = "Grow")
		unlucky_node.parent.replace_child(new_child=new_subtree, old_child=unlucky_node)
		new_individual = Individual(generation_of_creation = self.generation, genotype = tree_copy, n_objectives = self.n_objectives)
		return new_individual
		
	def generate_random_node(self):
		node = DT_Node()

		attribute = rd.choice([att for att in list(self.attributes.values()) if len(att.crucial_values) > 0]) #can be optimised
		operator_name = rd.choice(self.operators)
		operator = self.operators_db[operator_name]
		
		comparable_value_index = rd.choice(range(len(attribute.crucial_values))) #can be optimised
		comparable_value = attribute.crucial_values[comparable_value_index]
		node.update(attribute=attribute,
					comparable_value_index=comparable_value_index,
					comparable_value=comparable_value,
					operator=operator,
					operator_name=operator_name,
					output_labels=self.unique_output_labels)

		return node

	def generate_random_terminal(self):
		terminal_node = DT_Node()
		terminal_node.output_label = rd.choice(list(self.unique_output_labels))
		return terminal_node

	def generate_random_tree(self,max_depth=6, min_depth=4, current_depth=0, method = "Grow"): #missing adding names (might not be needed)
		if max_depth == current_depth:
			return self.generate_random_terminal()
		else:
			root = self.generate_random_node()
			if method == "Grow":
				for i in range(2):
					if current_depth < min_depth:
						need_terminal = False
					else:
						need_terminal = rd.choice([True,False])
					if need_terminal:
						child = self.generate_random_terminal()
					else:
						child = self.generate_random_tree(max_depth=max_depth, min_depth=min_depth, current_depth=current_depth+1, method = method)
						
					root.add_child(name=None,child=child)
		return root

	def evolve(self, generations = 1): #unfinished, also missing verification of repeated nodes
		for i_gen in range(int(generations)):
			self._run_generation()

	def _run_generation(self):
		"""
		Runs a single generation of evolution: updates self.population and evaluates the individuals
		Evaluates the population if generation == 0, otherwise assumes that the population is already evaluated
		"""
		if self.generation == 0:
			self.evaluate_population()
			self._fast_nondominated_sort()
			pop_size = len(self.population)
			self.crossovers = int(pop_size * self.crossover_rate / 2)
			self.mutations = int(pop_size * self.mutation_rate)
			self.elites = int(pop_size * self.elitism_rate)
			
		self.generation = self.generation + 1
		newgen_pop = []
		for i in range(self.crossovers):
			parent1 = self.tournament_selection()
			parent2 = self.tournament_selection()
			newgen_pop.extend(self._one_point_crossover(parent1, parent2))
		for i in range(self.mutations):
			parent = self.tournament_selection()
			#newgen_pop.append(self.mutate(parent))
			newgen_pop.append(self._subtree_mutation(parent))
		
		if self.n_objectives > 1:
			#Runs NSGA-II. Elitism is not taken into account
			pop_size = len(self.population)
			self.evaluate_population(population = newgen_pop)
			extended_population = self.population.extend(newgen_pop)
			self._fast_nondominated_sort(population = extended_population)
			sorted_competitors = self._multiobjective_sort_individuals(population = extended_population)
			self.population = sorted_competitors[:pop_size]
		else:
			sorted_competitors = self._sort_individuals()
			newgen_pop.extend(sorted_competitors[:self.elites])
			self.population = newgen_pop
			self.evaluate_population()
		
		self.current_generation = self.current_generation + 1

	def tournament_selection(self):
		"""
		Self.tournament_size individuals are randomly sampled from self.population.
		The fittest one in them is returned.
		"""
		competitors = rd.sample(self.population, self.tournament_size)
		winner = self.get_best_individual(population = competitors, objective_index = 0)
		return winner

	def evaluate_tree(self,root_node,data=None):
		"""
		return: list of output labels
		"""
		if data is None:
			data = self.data
		output_array = []
		root_node.reset_tree_numbers()
		for index, row in enumerate(data.iterrows()):
			#print("label", self.output_labels[index])
			output_array.append(root_node.evaluate(data_row=row[1], label=self.output_labels[index]))
		return output_array

	def calculate_accuracy(self, model_output_labels):
		corrects = 0
		for i,label in enumerate(self.output_labels):
			if label == model_output_labels[i]:
				corrects = corrects + 1
		accuracy = corrects / (i+1)
		return accuracy

	def evaluate_population(self, population = None):
		"""
		Updates the objective_values of all the individuals
		"""
		if population is None:
			population = self.population
		for objective_index, objective in self.objectives.items():
			for ind in population:
				if not ind.evaluated_on_static_objectives: #A boolean value that prevents from evaluating twice a same individual
					if objective.objective_name == "accuracy":
						labels = self.evaluate_tree(ind.genotype, data = self.data)
						objective_value = self.calculate_accuracy(model_output_labels=labels)
						#ind.objective_values[objective_index] = objective_value	 #will cause weird errors
						ind.objective_values = [old_value if i!=objective_index else objective_value for i,old_value in enumerate(ind.objective_values)]
					elif objective.objective_name == "nodes":
						objective_value = len(ind.genotype.get_subtree_nodes(include_terminals = False))
						#ind.objective_values[objective_index] = objective_value
						ind.objective_values = [old_value if i!=objective_index else objective_value for i,old_value in enumerate(ind.objective_values)]

		for ind in population:
			ind.evaluated_on_static_objectives = True

	def adapt_to_data(self, labels, data):
		"""
		Updates the dataset and adds attributes to play with
		"""
		self.update_dataset(labels = labels, data = data)
		for i, attribute in enumerate(self.data.columns):
			self.add_attribute(name = attribute, index = i)
		self.unique_output_labels = set(self.output_labels)
		print("Adapted to a dataset with ",str(len(self.attributes.keys()))," attributes and ",str(len(self.data.index))," rows")
		print("Classes: ",self.unique_output_labels)
		
	def update_dataset(self,labels,data):
		"""
		This functoin should only be used to insert data that has the same structure from the previous one (i.e. swapping train and test)
		"""
		self.data = pd.DataFrame(data)
		self.output_labels = list(labels)

	def remove_attribute(self,name): #CHANGE: missing
		pass

	def insert_r_tree_to_population(self, tree):
		parsed_tree = self._parse_tree_r(tree)
		self.insert_tree_to_population(parsed_tree)

	def insert_tree_to_population(self,tree):
		individual = Individual(generation_of_creation = self.current_generation,
								genotype = tree,
								n_objectives = self.n_objectives)
		self.population.append(individual)

	def _parse_tree_r(self, tree):
		pd_tree = pd.DataFrame(tree)
		labels = pd_tree["RHS"]
		rules = pd_tree["LHS"]
		#rules = rules[:20]                                   #for testing
		root_node = DT_Node()
		for rule_index,rule in enumerate(rules):
			#print("Parsing rule ", rule)
			node = root_node
			rule = str(rule)
			comparisons = rule.split(" & ")           #& symbol should not be in the name of any attribute, verify later
			n_comparisons = len(comparisons)-1
			for comp_idx,comparison in enumerate(comparisons):
				#print("parsing comparison ", comparison)
				if comparison in node.children_names:
					child_index = node.children_names.index(comparison)
					child = node.children[child_index]
				else:
					if node.updated: #an updated node has an assigned attribute
						pass
						#attribute = node.attribute
						#attribute, operator, value, value_index = self.parse_comparison(comparison, attribute=attribute)
						#if self.inverse_operators[operator] == operator:
						#	child = node.add_child(name=comparison)
						#pass
					else:
						attribute, operator, operator_name, comparable_value, comparable_value_index = self._parse_comparison(comparison)
						node.update(attribute=attribute,
									comparable_value_index=comparable_value_index,
									comparable_value=comparable_value,
									operator=operator,
									operator_name=operator_name,
									output_labels=self.unique_output_labels)
					child = node.add_new_child(name=comparison)
				#if n_comparisons == comp_idx:
				node = child
			#print("labels[rule_index]",labels[rule_index])
			#print("rule_index",rule_index)
			node.output_label = labels[rule_index]
			print("Parsed a tree with: ", len(root_node.get_subtree_nodes()), " nodes")
		return root_node

	def _parse_comparison(self,comparison):

		#Find the operator in the comparison string, can be made more robust
		for operator_name, operator in self.operators_db.items():
			operator_string = " " + operator_name + " "
			operator_string_index = comparison.find(operator_string)
			if operator_string_index != -1:
				break

		if operator_string_index == -1:
			print("Error, no operator from the DB was found in the rule string: ", comparison)

		#Split the string
		attribute_string = comparison[: operator_string_index]
		value_string = comparison[operator_string_index + len(operator_string) :]

		#Get variables
		if attribute_string in self.attributes.keys():
			attribute = self.attributes[attribute_string]
		else:
			print("Error, attribute not found: ", attribute_string)
		comparable_value = float(value_string) #all are floats because of this

		#Add to the model
		self.add_operator(operator_name)
		comparable_value_index = attribute.add_crucial_value(comparable_value)

		return attribute, operator, operator_name, comparable_value, comparable_value_index

	def get_attribute_names(self):
		at_names = [attribute.name for attribute in self.attributes.values()]
		return at_names

	def get_crucial_values(self):
		at_values = [attribute.crucial_values for attribute in self.attributes.values()]
		return at_values

	def get_best_individual(self, population = None, objective_index = None):
		if population is None:
			population = self.population
		if objective_index is None:
			if self.n_objectives > 1:
				sorted_competitors = self._multiobjective_sort_individuals(population = population)
			else:
				sorted_competitors = self._sort_individuals(population = population, objective_index = 0)
		else:
			sorted_competitors = self._sort_individuals(population = population, objective_index = int(objective_index))
		return sorted_competitors[0]

	def get_best_value_for_objective(self, population = None, objective_index = 0):
		objective_index = int(objective_index)
		if population is None:
			population = self.population
		winner = self.get_best_individual(population=population, objective_index = objective_index)
		return winner.objective_values[objective_index]

	def get_population_mean_for_objective(self, population = None, objective_index = 0):
		if population is None:
			population = self.population
		mean = sum([ind.objective_values[int(objective_index)] for ind in self.population])/len(self.population)
		return mean

	def _sort_individuals(self, population = None, objective_index = 0):
		"""
		Sorts the poopulation according to the objective object in self.objectives[objective_index]
		"""
		if population is None:
			population = self.population
		
		objective_index = int(objective_index)
		current_objective = self.objectives[objective_index]
		sorted_competitors = sorted(population, key=lambda ind: ind.objective_values[objective_index], reverse = current_objective.to_max)
		return sorted_competitors

	def _fast_nondominated_sort(self, population = None):
		"""
		Method proposed in the NSGA-II paper
		Updates self.fronts and the NSGA-II values in the individuals
		Is skipped if there are less than 2 objectives
		"""
		if self.n_objectives > 1:
			if population is None:
				population = self.population
				
			self.fronts = defaultdict(lambda:[])
			for p in population:
				p.dominated_solutions = []
				p.domination_counter = 0
				for q in population:
					if self._dominates(p,q):
						p.dominated_solutions.append(q)
					elif self._dominates(q,p):
						p.domination_counter = p.domination_counter + 1
				if p.domination_counter == 0:
					p.rank = 1
					self.fronts[1].append(p)
			
			front_index = 1
			while self.fronts[front_index] != []:
				temporal_front = []
				for p in self.fronts[front_index]:
					for q in p.dominated_solutions:
						q.domination_counter = q.domination_counter - 1
						if q.domination_counter == 0:
							q.rank = front_index + 1
							temporal_front.append(q)
				front_index = front_index + 1
				self.fronts[front_index] = temporal_front
				
			self._set_crowding_distances(population = population)
				
	def _set_crowding_distances(self, population = None):
		if population is None:
			population = self.population
		
		for individual in population:
			individual.crowding_distance = 0
		
		for objective_index in range(self.n_objectives):
			sorted_population = self._sort_individuals(population = population, objective_index = objective_index)
			best_individual = sorted_population[0]
			worst_individual = sorted_population[-1]
			best_value = best_individual.objective_values[objective_index]
			worst_value = worst_individual.objective_values[objective_index]
			gap = abs(best_value-worst_value)
			best_individual.crowding_distance = np.inf
			worst_individual.crowding_distance = np.inf
			for individual_index, individual in enumerate(sorted_population[1:-1]):
				individual.crowding_distance = individual.crowding_distance + abs((sorted_population[individual_index + 2].objective_values[objective_index] - sorted_population[individual_index].objective_values[objective_index])/gap)
	
	def _dominates(self, p, q):
		equals = 0
		for objective_index, objective in self.objectives.items():
			p_val = p.objective_values[objective_index]
			q_val = q.objective_values[objective_index]
			if objective.to_max:
				if p_val < q_val:
					return False
			else:
				if p_val > q_val:
					return False
		return True
		
	def _multiobjective_sort_individuals(self, population = None):
		"""
		Sorts the population according to the rank in first place, then the crowding distance.
		Sorting method's stability allows to separate the sort in twi steps
		"""
		if population is None:
			population = self.population
		sorted_individuals = sorted(population, key=op.attrgetter("crowding_distance"), reverse = True)
		sorted_individuals = sorted(sorted_individuals, key=op.attrgetter("rank"))
		return sorted_individuals

	def restart_evolution(self):
		self.current_generation = 0
		self.population = []
		self.archive_population = []
		self.hall_of_fame = []

	def save_tree(self, tree):
		pass
	
	def load_tree(self, tree):
		pass
