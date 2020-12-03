#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 17 23:15:50 2020
@author: Fred Valdez Ameneyro
Evolutionary Algorithm for Binary and Oblique Decision Trees
"""
import numpy as np
import random as rd
import operator as op
import math
import pandas as pd
import copy
import statistics as st
from collections import defaultdict
import pickle
import time
from datetime import datetime

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

class Individual:
	def __init__(
			self,
			generation_of_creation,
			genotype,
			n_objectives = 0,
			phenotype = None):

		self.generation_of_creation = generation_of_creation
		self.genotype = genotype
		self.phenotype = phenotype
		self.genotype_cleaned_and_reduced = False
		self.shrinked = False
		
		self.evaluated_on_static_objectives = False
		self.n_nodes = None
		self.max_depth = None
		self.rank = None                                 #used in NSGA-II
		self.dominated_solutions = []                    #used in NSGA-II
		self.domination_counter = None                   #used in NSGA-II
		self.crowding_distance = None                    #used in NSGA-II
		self.objective_values = [None for _ in range(n_objectives)]
		self.meets_constraints = None
		self.duplicated = False

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
		"""
		if value not in self.crucial_values:
			self.crucial_values = self.crucial_values + [value]
			print("Added crucial value ", str(value), " to attribute ", self.name)

	def get_random_crucial_value(self, values_to_exclude=[]):
		"""
		Returns a crucial value index and a crucial value at random, excluding the values_to_excludeto
		"""
		available_crucial_values = [cv for cv in self.crucial_values if cv not in values_to_exclude]
		if len(available_crucial_values) == 0:
			return None
		else:
			return rd.choice(available_crucial_values)

	def __str__(self):
		return "Attribute " + self.name + ",idx:" + str(self.index) + ",n_crucial_values:" + str(len(self.crucial_values))

class DT_Node:
	def __init__(self, unique_output_labels, parent = None):
		"""
		children are sorted: the first one is the true, the second one is the false
		the creation of an object inheriting this class must be updated with the self.update function
		"""
		#Initial values
		self.visits_count = 0
		self.gini_index = None
		self.entropy = None
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
		self.unique_output_labels = unique_output_labels
		self.output_label_count = {label:0 for label in unique_output_labels}

	def update(self,attribute,comparable_value,operator,operator_name, output_labels): #only used when parsing trees from R
		self.updated = True
		self.attribute = attribute
		self.comparable_value = comparable_value
		self.operator = operator
		self.operator_name = operator_name
		self.output_label_count = {label:0 for label in output_labels}

	def reset_numbers(self):
		self.visits_count = 0
		self.gini_index = None
		self.entropy = None
		self.output_label_count = {key:0 for key in self.output_label_count.keys()}

	def reset_tree_numbers(self):
		for node in self.get_subtree_nodes():
			node.reset_numbers()

	def add_child(self,name,child,index=None):
		child.parent = self
		if index is None:
			self.children = self.children + [child]
			self.children_names = self.children_names + [name]
		else:
			self.children = [c if i!=index else child for i,c in enumerate(self.children)]
			self.children_names = [n if i!=index else name for i,n in enumerate(self.children_names)]
			#self.children[index] = child
			#self.children_names[index] = name
		#print("Added child named ", name , ",new child count", str(len(self.children)))
		return child

	def add_new_child(self, name, unique_output_labels=None):
		if unique_output_labels is None:
			unique_output_labels = self.unique_output_labels
		empty_node = DT_Node(unique_output_labels=unique_output_labels)
		child = self.add_child(name=name,child=empty_node, index = None)
		return child

	def replace_child(self, new_child, old_child):
		index = self.children.index(old_child)
		self.add_child(name = self.children_names[index], child = new_child, index = index)

	def replace_self(self, new_node):
		self.update(attribute = new_node.attribute,
					comparable_value= new_node.comparable_value,
					operator=new_node.operator,
					operator_name= new_node.operator_name, 
					output_labels = new_node.unique_output_labels)
		self.reset_numbers()
		
	def get_entropy(self):
		if self.visits_count > 0:
			if 0 in [label_count for label_count in self.output_label_count.values()]:
				entropy = 0
			else:
				entropy = sum([-(label_count/self.visits_count)*math.log(label_count/self.visits_count,2) for label_count in self.output_label_count.values()])
			self.entropy = entropy
			return entropy
		else:
			return None

	def get_gini_index(self):
		if self.visits_count > 0:
			gini = 1 - sum([(label_count/self.visits_count)**2 for label_count in self.output_label_count.values()])
			self.gini_index = gini
			return gini
		else:
			return None

	def evaluate(self,data_row, label):
		self.visits_count = self.visits_count + 1
		self.output_label_count[label] = self.output_label_count[label] + 1
		if self.output_label is None:
			comparison = self.operator(data_row.iloc[self.attribute.index],self.comparable_value)
			
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

	def get_sibling(self): #not tested, not currently used
		if self.parent is None:
			return None
		else:
			for sibling in self.parent.children:
				if sibling != self:
					return sibling
			print("get_sibling: This should never be reached")
			return None
	
	def get_path(self):
		"""
		Returns a list with all the nodes that link self with the root (including self)
		"""
		node_list = [self]
		if not self.is_root():
			node_list.extend(self.parent.get_path())
		return node_list
	
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

	def get_subtree_leaves(self):
		return [node for node in self.get_subtree_nodes() if node.is_terminal()]

	def get_connections(self, include_terminals = True):
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
			return max([child.get_max_depth(depth=new_depth) for child in self.children])
			
	def get_min_depth(self, depth = 0):
		"""
		Returns the min depth of this tree as an int
		"""
		new_depth = depth + 1
		if self.is_terminal():
			return new_depth
		else:
			return min([child.get_min_depth(depth=new_depth) for child in self.children])

	def get_my_depth(self):
		path = self.get_path()
		return len(path)

	def node_already_in_branch(self, node=None): #not tested, not currently used
		if self.parent is None:
			return False
		else:
			if self == node:
				return True
			else:
				return parent.node_already_in_branch(node=node)
	
	def is_useful_split(self):
		for child in self.children:
			if child.visits_count == 0:
				return False
		return True
		
	def swap_output_label(self):
		for label in self.output_label_count.keys():
			if label != self.output_label:
				self.output_label = label
				break
	
	def copy(self, parent=None):
		"""
		Returns an unrelated new item with the same characteristics
		"""
		the_copy = DT_Node(parent = parent, unique_output_labels=self.unique_output_labels)
		the_copy.updated=self.updated
		the_copy.children_names = [n for n in self.children_names]
		the_copy.output_label = self.output_label
		the_copy.attribute = self.attribute
		the_copy.operator = self.operator
		the_copy.operator_name = self.operator_name
		the_copy.comparable_value = self.comparable_value
		
		the_copy.reset_numbers()

		for child in self.children:
			copy_child = child.copy(parent=the_copy)
			the_copy.children.append(copy_child)

		return the_copy

	def __str__(self):
		gini = self.get_gini_index()
		if gini is None:
			gini_string = "N/A"
		else:
			gini_string = "%.2f"%gini
		entropy = self.get_entropy()
		if entropy is None:
			entropy_string = "N/A"
		else:
			entropy_string = "%.2f"%entropy
		if self.visits_count > 0:
			output_label_rates = {k:"%.2f"%(count/self.visits_count) for k,count in self.output_label_count.items()}
		else:
			output_label_rates = {k:"0" for k,count in self.output_label_count.items()}
		output_label_strings = [str(k) + ":" + str(v) for k,v in output_label_rates.items()]
		output_label_string = output_label_strings[0] + "\n" + output_label_strings[1]
		trailing_string = "\n" + output_label_string + "\nGini:" + gini_string + "\nEntropy:" + entropy_string
		if self.is_terminal():
			return "Class: " + str(self.output_label) + trailing_string
		else:
			return self.attribute.name +"\n"+ self.operator_name + str(self.comparable_value) + trailing_string

class DecisionTree_EA: #oblique, binary trees
	def __init__(self,
				population_size = 100,
				tournament_size = 3, 
				crossover_rate = 0.5, 
				mutation_rate = 0.4, 
				elitism_rate = 0.1, 
				#hall_of_fame_size = 3,
				max_depth = 999,
				max_nodes = None,
				objective_names = [],
				uniform_mutation_rate = 0.05,
				forced_full = False,
				subtree_mutation_max_tree_depth = 4):

		self.output_labels = []
		self.unique_output_labels = []
		self.attributes = {} #{name : attribute class object}
		self.operators = []
		self.objectives = {} #{objective index : objective class object}
		self.n_objectives = 0
		self.generation = 0
		self.population = []
		self.parsing_operators = []
		self.operators_db = {"<":op.lt,">=":op.ge,"<=":op.le,">":op.gt,"==":op.eq,"!=":op.ne}
		self.inverse_operators = {op.lt:op.ge,op.le:op.gt,op.gt:op.le,op.ge:op.lt,op.eq:op.ne}
		
		self.population_size = int(population_size)
		self.tournament_size = int(tournament_size)
		self.archive_population = []
		#self.hall_of_fame_size = int(hall_of_fame_size)
		self.hall_of_fame = {} #{objective_index:[ind,ind...]}
		self.crossover_rate = crossover_rate
		self.mutation_rate = mutation_rate
		self.elitism_rate = elitism_rate
		self.crossovers = 0
		self.mutations = 0
		self.elites = 0
		self.fronts = defaultdict(lambda:[])                   #used in NSGA-II
		self.max_depth = max_depth
		self.max_nodes = max_nodes
		self.uniform_mutation_rate = uniform_mutation_rate
		self.forced_full = forced_full
		self.subtree_mutation_max_tree_depth = subtree_mutation_max_tree_depth
		
		#Initialisation
		#self.update_forced_balanced_trees()
		for objective_name in objective_names:
			self.add_objective(objective_name=objective_name)
		
		print("Called class DecisionTree_EA")

	#Class update and handling
	def add_operator(self,operator_name):
		if operator_name not in self.operators:
			self.operators.append(operator_name)
			print("Added new operator:",operator_name)

	def add_objective(self, objective_name, to_max = True, best = None, worst = None):
		"""
		Adds an objective with objective_name string as name
		Possible objectives_name are: "nodes", "max_depth", "accuracy", "entropy", "gini"
		"""
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

	def remove_attribute(self,attribute_index):
		"""
		Removes self.attributes[attribute_index]
		"""
		attribute_index = int(attribute_index)
		old_length = len(self.attributes)
		self.attributes = {key:val for key,val in self.attributes if key!=attribute_index}
		new_length = len(self.attributes)
		if old_length == new_length:
			print("There is no attribute with the index ", str(attribute_index))

	def remove_crucial_value(self,attribute_name,value):
		"""
		Removes the given value from the self.attributes[attribute_index]
		"""
		attribute_name = str(attribute_name)
		if attribute_name in self.attributes.keys():
			old_length = len(self.attributes[attribute_name].crucial_values)
			self.attributes[attribute_name].crucial_values = [cv for cv in self.attributes[attribute_name].crucial_values if cv != value]
			new_length = len(self.attributes[attribute_name].crucial_values)
			if new_length == old_length:
				print("The value ", str(value), " was not being considered or was not found")
			else:
				print("The value ", str(value), " was removed from the attribute", str(attribute_name))
			if new_length == 0:
				self.remove_attribute(attribute_index)
		else:
			print("There is no attribute with the name ", str(attribute_name))

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

	def insert_r_tree_to_population(self, tree):
		parsed_tree = self._parse_tree_r(tree)
		self.insert_tree_to_population(parsed_tree)

	def insert_tree_to_population(self,tree):
		individual = Individual(generation_of_creation = self.generation,
								genotype = tree,
								n_objectives = self.n_objectives)
		self.population.append(individual)

	def _parse_tree_r(self, tree):
		pd_tree = pd.DataFrame(tree)
		labels = pd_tree["RHS"]
		rules = pd_tree["LHS"]
		#rules = rules[:20]                                   #for testing
		root_node = DT_Node(unique_output_labels=self.unique_output_labels)
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
						attribute, operator, operator_name, comparable_value = self._parse_comparison(comparison)
						node.update(attribute=attribute,
									comparable_value=comparable_value,
									operator=operator,
									operator_name=operator_name,
									output_labels=self.unique_output_labels)
					child = node.add_new_child(name=comparison, unique_output_labels = self.unique_output_labels)
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
		attribute.add_crucial_value(comparable_value)

		return attribute, operator, operator_name, comparable_value

	def get_valid_attributes(self):
		"""
		Returns a list with the attributes in self.atttributes that have values to split
		"""
		return [att for att in list(self.attributes.values()) if len(att.crucial_values) > 0]

	def get_attribute_names(self):
		at_names = [attribute.name for attribute in self.attributes.values()]
		return at_names

	def get_crucial_values(self):
		at_values = [attribute.crucial_values for attribute in self.attributes.values()]
		return at_values

	def get_objective_names(self):
		names = [v.objective_name for k,v in self.objectives.items()]
		return names

	def get_filtered_individual_indexes(self, rank_threshold = 1):
		ranks = [ind.rank if not ind.rank is None else 0 for ind in self.population]
		return [str(i) if rank <= rank_threshold else None for i,rank in enumerate(ranks)]

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

	def get_tree_by_individual_index(self, index):
		index = int(index)
		if index < self.population_size:
			ind = self.population[index]
			tree = ind.genotype
			return tree
		return None

	#Genetic operators
	def _one_point_crossover(self, individual1, individual2):
		"""
		A random node in each tree is selected. The corresponding subtrees are swapped
		"""
		tree1 = individual1.genotype
		tree2 = individual2.genotype
		
		#tree1_max_depth = self.calculate_max_depth(individual=individual)
		#tree2_max_depth = self.calculate_max_depth(individual=individua2)

		copy1 = tree1.copy()
		copy2 = tree2.copy()

		#There is a 50% chance to consider terminals to randomly choose for the swap
		#include_terminals = rd.choice([True, False])
		nodes1 = copy1.get_subtree_nodes(include_self = False, include_terminals = False)
		nodes2 = copy2.get_subtree_nodes(include_self = False, include_terminals = False)
		
		#Ensuring constraints
		rd.shuffle(nodes1)
		rd.shuffle(nodes2)
		
		node1_found=False
		for node1 in nodes1:
			node1_path_depth = len(node1.get_path())
			node1_subtree_depth = node1.get_max_depth()
			
			node2_found = False
			for node2 in nodes2:
				node2_path_depth = len(node2.get_path())
				node2_subtree_depth = node2.get_max_depth()
				excess1 = (node1_path_depth + node2_subtree_depth - 1) - self.max_depth
				excess2 = (node2_path_depth + node1_subtree_depth - 1) - self.max_depth
				if excess1 <= 0 and excess2 <= 0:
					node2_found = True
					break
			
			if node2_found:
				node1_found = True
				break
		
		if not node1_found:
			print("No valid crossover point was found, replacing with random trees")
			node1 = self.generate_random_tree(method="Grow", max_depth=node2_subtree_depth)
			node2 = self.generate_random_tree(method="Grow", max_depth=node1_subtree_depth)

		#Replacing in the individuals
		temp1 = node1.copy()
		temp2 = node2.copy()
		node1.parent.replace_child(new_child=temp2, old_child=node1)
		node2.parent.replace_child(new_child=temp1, old_child=node2)

		new_individual1 = Individual(generation_of_creation = self.generation, genotype = copy1, n_objectives = self.n_objectives)
		new_individual2 = Individual(generation_of_creation = self.generation, genotype = copy2, n_objectives = self.n_objectives)

		return [new_individual1, new_individual2]

	def _single_point_mutation(self, individual):#not currently used
		"""
		Changes the attribute, operator and value of the randomly selected node in the tree.
		Terminals cannot be selected
		"""
		tree = individual.genotype
		tree_copy = tree.copy()
		nodes = tree_copy.get_subtree_nodes(include_self = False, include_terminals = False)
		unlucky_node = rd.choice(nodes)
		new_node = self.generate_random_node()
		#self._node_swap(old_node=unlucky_node, new_node=new_node)
		unlucky_node.parent.replace_child(new_child=new_node, old_child=unlucky_node)
		for child in unlucky_node.children:
			new_node.add_child(name="Noname",child=child)
		new_individual = Individual(generation_of_creation = self.generation, genotype = tree_copy, n_objectives = self.n_objectives)
		return new_individual
		
	def _subtree_mutation(self, individual, method = "Grow", subtree_max_depth=None, subtree_min_depth=1):
		"""
		Swaps a randomly selected subtree in the individual with a randomly generated tree.
		The random tree is generated using the method with the min and max depths
		If no max depth is given, it uses the max depth of the node it is replacing
		"""
		tree = individual.genotype
		tree_copy = tree.copy()
		nodes = tree_copy.get_subtree_nodes(include_self = True, include_terminals = True)
		unlucky_node = rd.choice(nodes)
		if subtree_max_depth is None:
			subtree_max_depth = unlucky_node.get_max_depth()
		new_subtree = self.generate_random_tree(max_depth=subtree_max_depth, min_depth = subtree_min_depth, method = method)
		if unlucky_node.is_root():
			tree_copy = new_subtree
		else:
			unlucky_node.parent.replace_child(new_child=new_subtree, old_child=unlucky_node)
		new_individual = Individual(generation_of_creation = self.generation, genotype = tree_copy, n_objectives = self.n_objectives)
		return new_individual
	
	def _uniform_mutation(self, individual, probability=None, include_terminals = False, safe_nodes = False):
		"""
		Each node in the tree is mutated with probability=probability
		A mutated node has its attribute, operator and value changed.
		The new attribute and value are not repeated in the path that link the node with the root to avoid redundancy
		"""
		if probability is None:
			probability=self.uniform_mutation_rate
		tree = individual.genotype
		tree_copy = tree.copy()
		nodes = tree_copy.get_subtree_nodes(include_self = True, include_terminals = include_terminals)
		for node in nodes:
			if rd.random() < probability:
				#print("mutated")
				if safe_nodes:
					new_node = self.generate_random_node_safe(old_node = node)
				else:
					new_node = self.generate_random_node()
				node.replace_self(new_node=new_node)
		new_individual = Individual(generation_of_creation = self.generation, genotype = tree_copy, n_objectives = self.n_objectives)
		new_individual.meets_constraints = individual.meets_constraints
		return new_individual
	
	def _generate_random_binary_list(self, length): #not tested, not currently used
		binary_list = [random.randint(0, 1) for _ in range(length)]
		return binary_list

	def _limited_depth_crossover(self, individual1, individual2):
		"""
		A random node in the first tree is selected. A node with the same depth in the second tree is selected to perform the swap
		"""
		tree1 = individual1.genotype
		tree2 = individual2.genotype

		copy1 = tree1.copy()
		copy2 = tree2.copy()

		nodes1 = copy1.get_subtree_nodes(include_self = False, include_terminals = False)
		node1 = rd.choice(nodes1)
	
		#Find a second node with the same depth as node1
		node2 = rd.choice(copy2.get_subtree_leaves())
		for _ in range(node1.get_max_depth()):
			if not node2.parent.is_root():
				node2 = node2.parent

		#Replacing in the individuals
		temp1 = node1.copy()
		temp2 = node2.copy()
		node1.parent.replace_child(new_child=temp2, old_child=node1)
		node2.parent.replace_child(new_child=temp1, old_child=node2)

		new_individual1 = Individual(generation_of_creation = self.generation, genotype = copy1, n_objectives = self.n_objectives)
		new_individual2 = Individual(generation_of_creation = self.generation, genotype = copy2, n_objectives = self.n_objectives)

		return [new_individual1, new_individual2]

	def generate_random_node_safe(self, old_node):
		"""
		Generates a random node.
		The attribute and value are not repeated in the path that links the old_node with the root to avoid redundancy
		"""
		if old_node is None:
			return self.generate_random_node()
		path = old_node.get_path()
		path_attributes = list(set([node.attribute for node in path]))
		#print("Path attributes:",str([at.name for at in path_attributes]))
		available_attributes = self.get_valid_attributes()
		
		while(True):
			#print("In loop")
			attribute = rd.choice(available_attributes)
			remove = False
			if attribute in path_attributes:
				if len(attribute.crucial_values)==1:
					remove = True
				else:
					path_attribute_values = list(set([node.comparable_value for node in path if node.attribute == attribute]))
					crucial_value = attribute.get_random_crucial_value(values_to_exclude=path_attribute_values)
					if crucial_value is None:
						remove = True
					else:
						break
			else:
				crucial_value = attribute.get_random_crucial_value()
				break
				
			if remove:
				available_attributes.remove(attribute)
			if len(available_attributes) == 0:
				print("No options available to generate new node")
				return None
		
		operator_name = rd.choice(self.operators)
		operator = self.operators_db[operator_name]
		
		node = DT_Node(unique_output_labels=self.unique_output_labels)
		node.update(attribute=attribute,
					comparable_value=crucial_value,
					operator=operator,
					operator_name=operator_name,
					output_labels=self.unique_output_labels)
		return node

	def generate_random_node(self):
		"""
		Generates a node wth random attribute, operator and value (chosen from the pool of posibilities)
		"""
		node = DT_Node(unique_output_labels=self.unique_output_labels)

		attribute = rd.choice(self.get_valid_attributes())
		operator_name = rd.choice(self.operators)
		operator = self.operators_db[operator_name]
		
		#comparable_value_index = rd.choice(range(len(attribute.crucial_values))) #can be optimised
		comparable_value = rd.choice(attribute.crucial_values)
		node.update(attribute=attribute,
					comparable_value=comparable_value,
					operator=operator,
					operator_name=operator_name,
					output_labels=self.unique_output_labels)

		return node

	def generate_random_terminal(self):
		terminal_node = DT_Node(unique_output_labels=self.unique_output_labels)
		terminal_node.output_label = rd.choice(list(self.unique_output_labels))
		return terminal_node

	def generate_random_tree(self,max_depth=6, min_depth=4, current_depth=0, method = "Grow", parent=None, safe=True): #missing adding names (might not be needed)
		if max_depth-1 == current_depth:
			return self.generate_random_terminal()
		else:
			if safe:
				root = self.generate_random_node_safe(old_node=parent)
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
						child = self.generate_random_tree(max_depth=max_depth, min_depth=min_depth, current_depth=current_depth+1, method = method, parent=root, safe=safe)
					root.add_child(name=None,child=child)
					
			elif method == "Full":
				for i in range(2):
					child = self.generate_random_tree(max_depth=max_depth, min_depth=min_depth, current_depth=current_depth+1, method = method, parent=root,safe=safe)
					root.add_child(name=None,child=child)
			else:
				print("Wrong method received in generate_random_tree: ", str(method))
		return root

	#Individuals handling
	def evolve(self, generations = 1):
		for i_gen in range(int(generations)):
			self._run_generation()

	def meets_constraints(self, ind):
		"""
		Returns a boolean that reflects if the individual meets the constraints
		"""
		if ind.meets_constraints is None:
			if self.forced_full:
				if self.calculate_max_depth(ind) != self.max_depth or self.max_depth != ind.genotype.get_min_depth():
					return False
			if self.max_nodes is not None:
				if self.calculate_nodes(ind) > self.max_nodes:
					ind.meets_constraints = False
					return False
			if self.max_depth is not None:
				if self.calculate_max_depth(ind) > self.max_depth:
					ind.meets_constraints = False
					return False
			ind.meets_constraints = True
			return True
		else:
			return ind.meets_constraints
	
	def reduce_and_update(self, individual):
		"""
		Shrinks the useless nodes in the individual's tree and updates the n_nodes if shrinked
		"""
		if not individual.genotype_cleaned_and_reduced:
			individual.genotype_cleaned_and_reduced = True
			shrinked, node = self._shrink_useless_nodes(node=individual.genotype)
			if shrinked:
				individual.genotype = node
				self.calculate_nodes(individual=individual, force_update = True)
				self.calculate_max_depth(individual=individual, force_update = True)
				
	def _shrink_useless_nodes(self, node):
		"""
		Nodes that do not split data are replaced by the descents that do so or by a terminal
		"""
		shrinked = False
		for child in node.children:
			if child.is_useful_split():
				temp_shrinked,_ = self._shrink_useless_nodes(child)
				if temp_shrinked==True:
					shrinked = True
			else:
				shrinked = True
				new_child = self._retrieve_useful_descent(node=child)
				#print("Replacement", new_child)
				node.replace_child(new_child=new_child, old_child=child)
		return shrinked, node

	def _retrieve_useful_descent(self, node):
		"""
		Returns the descent node that splits data or the terminal
		"""
		#print("Shrinking node", node)
		if node.is_terminal():
			return node
		elif node.is_useful_split():
			_,node = self._shrink_useless_nodes(node)
			return node
		else:
			useful_node = None
			for child in node.children:
				if child.visits_count > 0:
					useful_node = self._retrieve_useful_descent(child)
					break
			return useful_node
			
	def generate_random_individual(self):
		if self.forced_full:
			method = "Full"
		else:
			method = "Grow"
		ind = Individual(generation_of_creation = self.generation, genotype = self.generate_random_tree(method=method, max_depth=self.max_depth), n_objectives = self.n_objectives)
		return ind
		
	def _ensure_population_quality(self):
		"""
		Removes excess of individuals and those not meeting with the constraints
		Fills the population with random trees if needed
		"""
		self.population = [ind for ind in self.population if self.meets_constraints(ind)]
		missing = self.population_size - len(self.population)
		print("At generation", str(self.generation), " population: missing", missing)
		if missing > 0:
			self.population.extend([self.generate_random_individual() for _ in range(missing)])
			print(str(len(self.population)))
		elif missing < 0:
			self.population = self.population[:self.population_size]
	
	def initial_setup(self):
		print("Running initial setup")
		self._ensure_population_quality()
		self.evaluate_population()
		self._fast_nondominated_sort()
		#Calculate the number of individuals to be generated with each method
		self.crossovers = int(math.ceil(self.population_size * self.crossover_rate / 2))
		self.mutations = int(self.population_size * self.mutation_rate)
		self.elites = int(self.population_size * self.elitism_rate)
	
	def _run_generation(self):
		"""
		Runs a single generation of evolution: updates self.population and evaluates the individuals
		Evaluates the population if generation == 0, otherwise assumes that the population is already evaluated
		"""
		
		start_gen = time.time()
		if self.generation == 0: self.initial_setup()
		self.generation = self.generation + 1
		newgen_pop = []
		
		#Crossover
		start_crossovers = time.time()
		for i in range(self.crossovers):
			parent1 = self.tournament_selection()
			parent2 = self.tournament_selection()
			if self.forced_full and self.max_depth > 2:
				newgen_pop.extend(self._limited_depth_crossover(parent1, parent2))
			else:
				newgen_pop.extend(self._one_point_crossover(parent1, parent2))
		end_crossovers = time.time()
		print("Crossover time ", str(end_crossovers-start_crossovers))
		print("newgen_pop ", str(len(newgen_pop)))
		
		start_meet_constraints = time.time()
		newgen_pop = [ind for ind in newgen_pop if self.meets_constraints(ind)]
		end_meet_constraints = time.time()
		print("Meets_constraints time ", str(end_meet_constraints-start_meet_constraints))
		print("newgen_pop ", str(len(newgen_pop)))

		#Mutation
		start_mutation = time.time()
		for i in range(self.mutations):
			parent = self.tournament_selection()
			if self.forced_full:
				newgen_pop.append(self._subtree_mutation(parent, method="Full"))
			else:
				newgen_pop.append(self._subtree_mutation(parent, method="Grow", subtree_max_depth = self.subtree_mutation_max_tree_depth))
		end_mutation = time.time()
		print("Mutations time ", str(end_mutation-start_mutation))
		print("newgen_pop ", str(len(newgen_pop)))
		
		start_meet_constraints = time.time()
		newgen_pop = [ind for ind in newgen_pop if self.meets_constraints(ind)]
		end_meet_constraints = time.time()
		print("Meets_constraints time ", str(end_meet_constraints-start_meet_constraints))
		print("newgen_pop ", str(len(newgen_pop)))
		
		#Posterior mutation
		start_uniform_mutation = time.time()
		if self.uniform_mutation_rate > 0:
			newgen_pop = [self._uniform_mutation(individual = ind, safe_nodes=True) for ind in newgen_pop]
		end_uniform_mutation = time.time()
		print("Uniform mutation time ", str(end_uniform_mutation-start_uniform_mutation))
		
		#Multi-objective evolution. Runs NSGA-II. Elitism is not taken into account
		if self.n_objectives > 1:
			self.evaluate_population(population = newgen_pop)
			extended_population = self.population.extend(newgen_pop)
			start_fastnondominatedsort = time.time()
			self._fast_nondominated_sort(population = extended_population)
			end_fastnondominatedsort = time.time()
			print("Fast non-dominated sort time ", str(end_fastnondominatedsort-start_fastnondominatedsort))
			sorted_competitors = self._multiobjective_sort_individuals(population = extended_population)
			self.population = sorted_competitors[:self.population_size]
			
		#Single objective evolution
		else: 
			sorted_competitors = self._sort_individuals()
			newgen_pop.extend(sorted_competitors[:self.elites])
			#Ensure that the population size is kept
			if len(newgen_pop) > self.population_size:
				additional = len(newgen_pop) - self.population_size
				newgen_pop = newgen_pop[additional:]
			elif len(newgen_pop) < self.population_size:
				missing = self.population_size - len(newgen_pop)
				newgen_pop.extend(sorted_competitors[self.elites:(self.elites+missing)])
			self.population = newgen_pop
			self.evaluate_population()
		end_gen = time.time()
		print("Gen time ", str(end_gen - start_gen))

	def tournament_selection(self):
		"""
		Self.tournament_size individuals are randomly sampled from self.population.
		The fittest one in them is returned.
		"""
		competitors = rd.sample(self.population, self.tournament_size)
		winner = self.get_best_individual(population = competitors, objective_index = 0)
		return winner

	def evaluate_tree(self,root_node,data=None, output_labels = None):
		"""
		return: list of output labels
		data should be a dataframe, output_labels a list. If not specified, the training data is used
		"""
		if data is None:
			data = self.data
		if output_labels is None:
			output_labels = self.output_labels
		output_array = []
		root_node.reset_tree_numbers()
		for index, row in enumerate(data.iterrows()):
			#print("label", self.output_labels[index])
			output_array.append(root_node.evaluate(data_row=row[1], label=output_labels[index]))
		return output_array

	def calculate_accuracy(self, model_output_labels, compare_labels = None):
		if compare_labels is None:
			compare_labels = self.output_labels
		corrects = 0
		for i,label in enumerate(compare_labels):
			if label == model_output_labels[i]:
				corrects = corrects + 1
		accuracy = corrects / (i+1)
		return accuracy

	def calculate_weighted_entropy(self, tree):
		leaves = tree.get_subtree_leaves()
		weighted_sum = sum([leaf.get_entropy()*leaf.visits_count if leaf.get_entropy() is not None else 0 for leaf in leaves])
		if tree.visits_count != 0:
			objective_value = weighted_sum/tree.visits_count
		else:
			objective_value = 1
			print("This should not be reached. Attempted to calculate entropy with a tree that was not evaluated in a dataset with at least one row")
		return objective_value
	
	def calculate_weighted_gini(self, tree):
		leaves = tree.get_subtree_leaves()
		weighted_sum = sum([leaf.get_gini_index()*leaf.visits_count if leaf.get_gini_index() is not None else 0 for leaf in leaves])
		if tree.visits_count != 0:
			objective_value = weighted_sum/tree.visits_count
		else:
			objective_value = 1
			print("This should not be reached. Attempted to calculate gini index with a tree that was not evaluated in a dataset with at least one row")
		return objective_value

	def calculate_nodes(self, individual, force_update = False):
		"""
		Returns the number of nodes in the individual's genotype and updates its the n_nodes variable
		"""
		if individual.n_nodes is None or force_update:
			n_nodes = len(individual.genotype.get_subtree_nodes(include_terminals = False))
			individual.n_nodes = n_nodes
		return individual.n_nodes

	def calculate_max_depth(self, individual, force_update = False):
		"""
		Returns the max depth in the individual's genotype and updates its the n_nodes variable
		"""
		if individual.max_depth is None or force_update:
			max_depth = individual.genotype.get_max_depth()
			individual.max_depth = max_depth
		return individual.max_depth

	def evaluate_population(self, population = None, data = None):
		"""
		Updates the objective_values of all the individuals
		"""
		if population is None:
			population = self.population
		if data is None:
			data = self.data
		
		start_eval = time.time()
		for ind in population:
			if not ind.evaluated_on_static_objectives: #A boolean value that prevents a same individual from being evaluated twice
				labels = self.evaluate_tree(ind.genotype, data = data)
				for objective_index, objective in self.objectives.items():
					if objective.objective_name == "accuracy":
						objective_value = self.calculate_accuracy(model_output_labels=labels)
					elif objective.objective_name == "nodes":
						objective_value = self.calculate_nodes(individual=ind)
					elif objective.objective_name == "max_depth":
						objective_value = self.calculate_max_depth(individual=ind)
					elif objective.objective_name == "entropy":
						objective_value = self.calculate_weighted_entropy(tree=ind.genotype)
					elif objective.objective_name == "gini":
						objective_value = self.calculate_weighted_gini(tree=ind.genotype)
					#ind.objective_values[objective_index] = objective_value #<-this way might cause changes to the class
					ind.objective_values = [old_value if i!=objective_index else objective_value for i,old_value in enumerate(ind.objective_values)]
		end_eval = time.time()
		print("Evaluation time ",str(end_eval-start_eval))
		
		#Reduces the individuals if they have nodes with visits=0. Updates the depth and nodes if altered
		start_reduce = time.time()
		for ind in population:
			#if ind.rank == 1:
			if not self.forced_full:
				self.reduce_and_update(individual=ind)
			ind.evaluated_on_static_objectives = True
		end_reduce = time.time()
		print("Reduce time ",str(end_reduce-start_reduce))

	def get_test_values(self, test_data, test_labels, individual=None, individual_index = None):
		if individual is None:
			if individual_index is not None:
				individual = self.population[individual_index]
			else:
				print("Individual not specified")
				return None
		data = pd.DataFrame(test_data)
		labels = list(labels)
		copy_tree = individual.genotype.copy()
		tree_output_labels = self.evaluate_tree(root_node=copy_tree, data=data)
		accuracy = self.calculate_accuracy(model_output_labels = tree_output_labels)
		entropy = self.calculate_weighted_entropy(tree=copy_tree)
		gini = self.calculate_weighted_gini(tree=copy_tree)
		return accuracy, entropy, gini
	
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
			if gap != 0:
				for individual_index, individual in enumerate(sorted_population[1:-1]):
					individual.crowding_distance = individual.crowding_distance + abs((sorted_population[individual_index + 2].objective_values[objective_index] - sorted_population[individual_index].objective_values[objective_index])/gap)
			else:
				pass
	
		irrelevants = [ind for ind in self.population if ind.crowding_distance==0]
		for i,p in enumerate(irrelevants):
			if not p.duplicated:
				rest = irrelevants[:i]+irrelevants[i+1:]
				for q in rest:
					if p.objective_values == q.objective_values:
						q.duplicated = True
				
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
		sorted_individuals = sorted(sorted_individuals, key=op.attrgetter("duplicated"))
		return sorted_individuals

	def restart_evolution(self):
		self.generation = 0
		self.population = []
		self.archive_population = []
		self.hall_of_fame = []

	def save_tree(self, tree):
		now = datetime.now()
		id = now.strftime("_%d-%m-%Y_%H-%M")
		filename = "tree" + id
		outfile = open(filename,'wb')
		pickle.dump(tree, outfile)
		outfile.close()
		return id
	
	def load_tree(self, filename=None, id=None):
		infile = open(filename,'rb')
		tree = pickle.load(infile)
		infile.close()
		return tree
