import numpy as np
import numpy.ma as ma
import jinja2
import csv
import matplotlib as plt

data_file = "./data/income_dist.csv"

with open(data_file, 'r') as csvfile:
	reader = csv.DictReader(csvfile)
	data = list(reader)