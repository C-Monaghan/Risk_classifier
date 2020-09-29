#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 17 23:15:50 2020

@author: Fred Valdez Ameneyro

#My Evolutionary Algorithm for Decision Trees
"""
import numpy as np
import random as rd
import operator as op
import math
from inspect import signature

def get_arity(self, operator):
		"""
		Returns the arity of the method, operator or funtion as an int
		:param operator: is a method, operator or funtion
		"""
		sig = signature(operator)
		arity = len(sig.parameters)
		return arity
		
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
		
class Individual:
	def __init__(
		self,
		generation_of_creation,
		genotype, 
		phenotype = None,
		objective_values = None,
		n_dominators = None,                 #used in MOEA, pareto dominance
		n_dominated_solutions = None,        #used in MOEA, pareto dominance
		dominated_solutions = None,          #used in NSGA-II
		local_crowding_distance = None,      #used in NSGA-II
		non_domination_rank = None,          #used in NSGA-II):

		self.generation_of_creation = generation_of_creation,
		self.genotype = genotype
		self.phenotype = phenotype
		self.n_dominators = n_dominators
		self.n_dominated_solutions = n_dominated_solutions
		self.dominated_solutions = dominated_solutions
		self.objective_values = objective_values
		self.local_crowding_distance = local_crowding_distance
		self.non_domination_rank = non_domination_rank
		
		self.n_objectives = len(objective_values)
		
class Attribute:
	def __init__(self,
		index,                        #index of the attribute in the dataset
		type,
		crucial_values = None,
		available_values = None)
	
	self.type = type
	self.crucial_values = crucial_values
	self.available_values = available_values
	
class DT_Node:
	def __init__(self, 
			attribute,
			comparable_value_index,
			operator,
			parent = None,
			children = []):

		self.attribute = attribute
		self.comparable_value = self.attribute.crucial_values[comparable_value_index]
		self.parent = parent
		self.children = children

	def is_terminal(self):
		return children == []

	def is_root(self):
		return self.parent is None

	def get_subtree_nodes(self):
		"""
		Returns a list with all the nodes of the subtree with this node as the root node, including himself
		"""
		nodes = [self]
		i = 0
		while i < len(nodes):
			if not nodes[i].is_terminal:
				nodes.extend(nodes[i].children)
			i += 1
		return nodes

	def get_max_depth(self, depth = 0):
		"""
		Returns the max depth of this tree as an int
		"""
		new_depth = depth + 1
		if self.is_terminal:
			return new_depth
		else:
			return max([child.my_depth(new_depth) for child in self.children])

	def copy(self, parent=None):
		"""
		Don't give arguments. Returns an unrelated new item with the same characteristics
		"""
		the_copy = GP_Node(self.content, parent = parent)
		if not self.is_terminal:
			for child in self.children:
				the_copy.children.append(child.copy(parent = the_copy))
		return the_copy

	def __str__(self):
			return name_string

class DecisionTree_EA:
	def __init__(self,
			output_labels = [0,1],
			tree_type = "oblique"):
		
		"""
		The EA needs attributes and objectives to be added with the add_attribute and add_objective functions
		:param output_labels are all possible output labels
		:param tree_type can be oblique or axis-parallel
		"""
		self.output_labels = output_labels
		self.attributes = {}
		self.objectives = {}
		self.n_objectives = 0
		self.generation = 0
		self.population = []
		
	def add_attribute(self,
			name,
			type,
			crucial_values = None,          #for numerical attributes
			available_values = None):       #for categorical attributes
		self.attributes[name] = Attribute(type=type,
										crucial_values=crucial_values,
										available_values=available_values)
										
	def add_objective(self,
			to_max = True,
			best_known_value = None,
			worst_known_value = None):
		self.objectives[self.n_objectives] = Objective(to_max = to_max,
													best_known_value = best_known_value,
													worst_known_value = worst_known_value):
		self.n_objectives = self.n_objectives + 1
	
	def create_individual()

	def attribute_mutation(self,
			individual):
		pass

	def attribute_value_mutation(self,
			individual):
		pass
			
	def one_point_crossover(self,
			individual1,
			individual2):
		pass