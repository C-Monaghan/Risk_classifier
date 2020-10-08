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

def get_arity(operator):
	"""
	Returns the arity of the method, operator or funtion as an int
	:param operator: is a method, operator or funtion
	"""
	sig = signature(operator)
	arity = len(sig.parameters)
	return arity
	
def add_crucial_value(attribute,value):
	"""
	crucial values are the relevant values that are used to split the data in the trees
	returns: the index in the attribute of the newly added value
	"""
	if value in attribute.crucial_values:
		index = attribute.crucial_values.index(value)
		return index
	else:
		index = len(attribute.crucial_values)
		attribute.crucial_values = attribute.crucial_values + [value]
		print("Added crucial value ", str(value), " to attribute ", attribute.name, ". All values:", str(attribute.crucial_values))
		return index
		
class Objective:
	def __init__(self,
		to_max = True,
		best_known_value = None,
		worst_known_value = None):
		self.index = index
		self.to_max = to_max
		self.best_known_value = best_known_value
		self.worst_known_value = worst_known_value
	
	def set_best_known_value(value):
		self.best_known_value = value
	
	def set_worst_known_value(value):
		self.worst_known_value = value
		
class Individual: #missing __lt__ and __eq__
	def __init__(
			self,
			generation_of_creation,
			genotype, 
			phenotype = None,
			objective_values = [0],
			n_dominators = None,                 #used in MOEA, pareto dominance
			n_dominated_solutions = None,        #used in MOEA, pareto dominance
			dominated_solutions = None,          #used in NSGA-II
			local_crowding_distance = None,      #used in NSGA-II
			non_domination_rank = None):          #used in NSGA-II

		self.generation_of_creation = generation_of_creation
		self.genotype = genotype
		self.phenotype = phenotype
		self.n_dominators = n_dominators
		self.n_dominated_solutions = n_dominated_solutions
		self.dominated_solutions = dominated_solutions
		self.objective_values = objective_values
		self.local_crowding_distance = local_crowding_distance
		self.non_domination_rank = non_domination_rank
	
	def __str__(self):
		return "Genotype:" + str(self.genotype)
	
class Attribute:
	def __init__(self,
			index,                        #index of the attribute in the dataset
			name,
			type = None,
			crucial_values = [],
			all_values = [],
			available_values = []):
		"""
		s
		"""
	
		self.index = index
		self.name = name
		self.type = type
		self.all_values = all_values
		self.crucial_values = crucial_values
		self.available_values = available_values
	
	def __str__(self):
		return "Attribute " + self.name + ",idx:" + str(self.index) + ",n_crucial_values:" + str(len(self.crucial_values))  
	
class DT_Node:
	def __init__(self, parent = None):
		"""
		children are sorted: the first one is the true, the second one is the false
		"""
		self.updated=False #only used when parsing trees from R
		self.parent=parent
		self.children = []
		self.children_names = []
		self.output_label = None
		self.attribute = None
		self.operator = None
		self.comparable_value = None
		self.comparable_value_index = None
		self.visits_count = 0
		self.gini_index = None
	
	def update(self,attribute,comparable_value_index,comparable_value,operator): #only used when parsing trees from R
		self.updated = True
		self.attribute = attribute
		self.comparable_value = comparable_value #float(self.attribute.crucial_values[comparable_value_index]) #all are floats rn
		self.comparable_value_index = comparable_value_index
		self.operator = operator
	
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
	
	def evaluate(self,data_row):
		self.visits_count = self.visits_count + 1
		if self.output_label is None:
			try:
				comparison = self.operator(data_row.iloc[self.attribute.index],self.comparable_value)
			except:
				print("Error in evaluation")
				return None
			
			if comparison == True:
				#print("Going to child", self.children_names[0])
				return self.children[0].evaluate(data_row)
			else:
				#print("Going to child", self.children_names[1])
				return self.children[1].evaluate(data_row)
			
		else:
			#print("Output:",self.output_label)
			return self.output_label

	def is_terminal(self):
		return self.children == []

	def is_root(self):
		return self.parent is None

	def get_subtree_nodes(self, include_self = True, include_terminals = True):
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

	def get_max_depth(self, depth = 0):
		"""
		Returns the max depth of this tree as an int
		"""
		new_depth = depth + 1
		if self.is_terminal():
			return new_depth
		else:
			return max([child.my_depth(new_depth) for child in self.children])

	def node_already_in_branch(self, node=None):
		if self.parent is None:
			return False
		else:
			if self == node:
				return True
			else:
				return parent.node_already_in_branch(node=node)

	def copy(self, parent=None): #missing test: evaluate the copy
		"""
		Don't give arguments. Returns an unrelated new item with the same characteristics
		"""
		the_copy = DT_Node(parent = parent)
		the_copy.updated=self.updated
		the_copy.children_names = [n for n in self.children_names]
		the_copy.output_label = self.output_label
		the_copy.attribute = self.attribute
		the_copy.operator = self.operator
		the_copy.comparable_value = self.comparable_value
		the_copy.comparable_value_index = self.comparable_value_index
		
		for child in self.children:
			copy_child = child.copy(parent=the_copy)
			the_copy.children.append(copy_child)
			#the_copy.add_child(name=self.children_names[child_index], child=copy_child)
				
		return the_copy
	
	def __str__(self):
			if self.attribute is None:
				return "Node: terminal,ol:" + str(self.output_label) + ",visits:" + str(self.visits_count)
			else:
				return "Node:" + self.attribute.name + ",op:" + str(self.operator) + ",value:" + str(self.comparable_value) + ",subtree nodes:" + str(len(self.get_subtree_nodes(include_terminals=False)))  + ",visits:" + str(self.visits_count)

class DecisionTree_EA: #oblique, binary trees
	def __init__(self, tournament_size = 3, crossover_rate = 0.5, mutation_rate = 0.4, elitism_rate = 0.1, hall_of_fame_size = 3):
			
		self.output_labels = []
		self.current_generation = 0
		self.unique_output_labels = []
		self.attributes = {}
		self.operators = []
		#self.objectives = {}
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
										
	def add_operator(self,operator): #operators with compatibility to certain attributes?
		if operator not in self.operators:
			self.operators.append(operator)
			print("Added new operator:",str(operator))
	
	def add_objective(self,
			to_max = True,
			best_known_value = None,
			worst_known_value = None):
		self.objectives[self.n_objectives] = Objective(to_max = to_max,
													best_known_value = best_known_value,
													worst_known_value = worst_known_value)
		self.n_objectives = self.n_objectives + 1

	def attribute_mutation(self,
			individual):
		pass

	def attribute_value_mutation(self,
			individual):
		pass
	
	def one_point_crossover(self, individual1, individual2):
		tree1 = individual1.genotype
		tree2 = individual2.genotype

		copy1 = tree1.copy()
		copy2 = tree2.copy()
		
		nodes1 = copy1.get_subtree_nodes(include_self = False, include_terminals = False) #no root node included
		nodes2 = copy2.get_subtree_nodes(include_self = False, include_terminals = False)
		
		node1 = rd.choice(nodes1)
		node2 = rd.choice(nodes2)

		temp1 = node1.copy()
		temp2 = node2.copy()
		node1.parent.replace_child(new_child=temp2, old_child=node1)
		node2.parent.replace_child(new_child=temp1, old_child=node2)

		new_individual1 = Individual(generation_of_creation = self.generation, genotype = copy1)
		new_individual2 = Individual(generation_of_creation = self.generation, genotype = copy2)

		return [new_individual1, new_individual2]
		
	def generate_name_for_node(self): #missing, right now names are only relevant when parsing trees from R
		pass
		
	def mutate(self, individual):#missing, new nodes have no names for their children
		tree = individual.genotype
		tree_copy = tree.copy()
		nodes = tree_copy.get_subtree_nodes(include_self = False, include_terminals = False)
		unlucky_node = rd.choice(nodes)
		new_node = self.generate_random_node()
		unlucky_node.parent.replace_child(new_child=new_node, old_child=unlucky_node)
		for child in unlucky_node.children:
			new_node.add_child(name="Noname",child=child) #new_node.add_child(name=self.generate_name_for_node...,child)
		#print("unlucky_node",unlucky_node)
		#print("new_node",new_node)
		#print("acc_old",str(self.calculate_accuracy(self.evaluate_tree(tree))))
		#print("acc_new",str(self.calculate_accuracy(self.evaluate_tree(tree_copy))))
		new_individual = Individual(generation_of_creation = self.generation, genotype = tree_copy)
		return new_individual
	
	def generate_random_node(self):
		node = DT_Node()
		node.updated = True #might not be needed
		node.attribute = rd.choice([att for att in list(self.attributes.values()) if len(att.crucial_values) > 0]) #can be optimised
		node.operator = rd.choice(self.operators)
		#if len(self.attributes.crucial_values) > 0:
		node.comparable_value_index = rd.choice(range(len(node.attribute.crucial_values))) #can be optimised
		node.comparable_value = node.attribute.crucial_values[node.comparable_value_index]
		#else:
			
		return node
	
	def generate_random_terminal(self):
		terminal_node = DT_Node()
		self.output_label = rd.choice(self.unique_output_labels)
		return terminal_node
		
	def evolve(self, generations = 1): #unfinished, also missing verification of existent nodes
		for i_gen in range(int(generations)):
			self.evaluate_population()
			pop_size = len(self.population)
			crossovers = int(pop_size * self.crossover_rate / 2)
			mutations = int(pop_size * self.mutation_rate)
			elites = int(pop_size * self.elitism_rate)
			newgen_pop = []
			for i in range(crossovers):
				parent1 = self.tournament_selection()
				parent2 = self.tournament_selection()
				newgen_pop.extend(self.one_point_crossover(parent1, parent2))
			for i in range(mutations):
				parent = self.tournament_selection()
				newgen_pop.append(self.mutate(parent))
			sorted_competitors = sorted(self.population, key=lambda ind: ind.objective_values[0], reverse = True)
			
			print("Gen ", str(self.generation), "Best so far:", str(sorted_competitors[0].objective_values[0]),str(sorted_competitors[1].objective_values[0]))
			for i in range(10):
				print(str(sorted_competitors[i].objective_values[0]),str(sorted_competitors[i].genotype))
			if elites > 0: #missing hall of fame
				newgen_pop.extend(sorted_competitors[:elites])
			self.population = newgen_pop
			self.generation = self.generation + 1
		self.evaluate_population()
		sorted_competitors = sorted(self.population, key=lambda ind: ind.objective_values[0], reverse = True)
		print("Gen ", str(self.generation), "Best so far:", sorted_competitors[0].objective_values[0])
		return sorted_competitors[0]
	
	#def plot_tree(self,
	
	def tournament_selection(self): #population could be sorted before to avoid repetition
		competitors = rd.sample(self.population, self.tournament_size)
		sorted_competitors = sorted(competitors, key=lambda ind: ind.objective_values[0], reverse = True)
		winner = sorted_competitors[0]
		return winner
		
	def evaluate_tree(self,root_node,provisional_data=None):
		"""
		return: list of output labels
		"""
		if provisional_data is None:
			data = self.data
		else:
			data = provisional_data
		output_array = []
		#restart the visit count in each node
		tree_family = root_node.get_subtree_nodes()
		for node in tree_family:
			node.visits_count = 0
		
		for index, row in data.iterrows():
			output_array.append(root_node.evaluate(row))
		return output_array
	
	def calculate_accuracy(self, model_output_labels):
		corrects = 0
		for i,label in enumerate(self.output_labels):
			if label == model_output_labels[i]:
				corrects = corrects + 1
		accuracy = corrects / (i+1)
		return accuracy
				
	def evaluate_population(self):
		#get individual objective values:
		for ind in self.population:
			labels = self.evaluate_tree(ind.genotype)
			accuracy = self.calculate_accuracy(model_output_labels=labels)
			#print(accuracy)
			ind.objective_values = [accuracy]
			#ind.objective_values[0] = accuracy how come this didn´t work
			
	def adapt_to_data(self, labels, data): #missing test_data
		self.data = pd.DataFrame(data)
		for i, attribute in enumerate(self.data.columns):
			self.attributes[attribute] = Attribute(index=i, name=attribute)
			print("added attribute",attribute)
		self.output_labels = list(labels)
		self.unique_output_labels = set(list(labels))
		self.dataset = data
		print("unique_output_labels",self.unique_output_labels)
	
	def insert_r_tree_to_population(self, tree):
		parsed_tree = self.parse_tree_r(tree)
		individual = Individual(generation_of_creation = self.current_generation,
								genotype = parsed_tree)
		self.population.append(individual)
	
	def parse_tree_r(self, tree):
		pd_tree = pd.DataFrame(tree)
		labels = pd_tree["RHS"]
		rules = pd_tree["LHS"]
		#rules = rules[:20]                                   #for testing
		root_node = DT_Node()
		for rule_index,rule in enumerate(rules):
			print("Parsing rule ", rule)
			node = root_node
			rule = str(rule)
			comparisons = rule.split(" & ")           #& symbol should not be in the name of any attribute, verify later
			n_comparisons = len(comparisons)-1
			for comp_idx,comparison in enumerate(comparisons):
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
						attribute, operator, comparable_value, comparable_value_index = self.parse_comparison(comparison)
						node.update(attribute=attribute,
									comparable_value_index=comparable_value_index,
									comparable_value=comparable_value,
									operator=operator)
					child = node.add_new_child(name=comparison)
				#if n_comparisons == comp_idx:
				node = child
			#print("labels[rule_index]",labels[rule_index])
			#print("rule_index",rule_index)
			node.output_label = labels[rule_index]
		return root_node
	
	def parse_comparison(self,comparison,attribute=None):
		#print("parsing comparison ", comparison)
		
		#Search for the attribute
		if attribute is None:
			for attribute_n,attribute_c in self.attributes.items():
				str_index = comparison.find(attribute_n)
				if str_index != -1:
					attribute = attribute_c
					break
		else:
			str_index = comparison.find(attribute.name)
			if str_index == -1:
				print("Error, wrong attribute")
			
		if attribute is None:
			print("Error, no attribute matched")
			
		#Get variables
		attribute_name = attribute.name
		remainder = comparison[str_index+len(attribute_name):].strip()
		operator = self.operators_db[remainder.split(" ")[0]]
		comparable_value = float(remainder.split(" ")[1]) #all are floats because of this
		
		#Add to the model
		self.add_operator(operator)
		comparable_value_index = add_crucial_value(attribute,comparable_value)
		
		return attribute, operator, comparable_value, comparable_value_index
