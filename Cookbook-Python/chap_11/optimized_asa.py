#!/usr/bin/env python

"""
Routines to calculate the Accessible Surface Area of a set of atoms.
The algorithm is adapted from the Rose lab's chasa.py, which uses
the dot density technique found in:

Shrake, A., and J. A. Rupley. "Environment and Exposure to Solvent
of Protein Atoms. Lysozyme and Insulin." JMB (1973) 79:351-371.
"""

import math
import numpy as np
from vector3d import pos_distance, Vector3d, pos_distance_sq
from scipy.spatial.distance import cdist


def generate_sphere_points(n):
    """
    Returns list of 3d coordinates of points on a sphere using the
    Golden Section Spiral algorithm.
    """
    points = []
    inc = math.pi * (3 - math.sqrt(5))
    offset = 2 / float(n)
    for k in xrange(int(n)):
        y = (k + .5) * offset - 1
        r = math.sqrt(1 - y*y)
        phi = k * inc
        points.append([math.cos(phi)*r, y, math.sin(phi)*r])
    return points

# @profile
def find_neighbor_indices(points, radii, probe, k):
    """
    Returns list of indices of atoms within probe distance to atom k. 
    """
    neighbor_indices = []
    radius = radii[k] + probe + probe
    test_radii = (radius + radii)**2
    dist_sq = np.sum(((points - points[k, :])**2), axis=1)
    
    neighbor_indices = (dist_sq < test_radii)
    neighbor_indices[k] = False

    return neighbor_indices

# @profile
def calculate_asa(atoms, probe, n_sphere_point=960):
    """
    Returns list of accessible surface areas of the atoms, using the probe
    and atom radius to define the surface.
    """
    sphere_points = generate_sphere_points(n_sphere_point)

    points = np.array([ [a.pos.x, a.pos.y, a.pos.z] for a in atoms ]) #, dtype=np.float16) 
    radii = np.array([a.radius for a in atoms]) #, dtype=np.float16)
  
    radii_probe = radii + probe
    radii_probe_sq = (radii_probe)**2

    const = 4.0 * math.pi / float(n_sphere_point)

    num_atoms = len(atoms)
    areas = np.zeros(num_atoms)

    for i in xrange(0, num_atoms):

        neighbor_indices = find_neighbor_indices(points, radii, probe, i)
        test_sphere_points = sphere_points*radii_probe[i] + points[i,:]
        neighbor_radii_sq = radii_probe_sq[neighbor_indices]
        diff_sq = cdist(test_sphere_points, points[neighbor_indices, :],
          'sqeuclidean')
        diff_test = (diff_sq < neighbor_radii_sq)
        inaccessible_points = np.any(diff_test, 1)
        areas[i] = np.sum(inaccessible_points)


    areas = (n_sphere_point-areas)*const*radii_probe_sq
    return areas


def main():
  import sys
  import getopt
  import molecule
  

  usage = \
  """

  Copyright (c) 2007 Bosco Ho
  
  Calculates the total Accessible Surface Area (ASA) of atoms in a 
  PDB file. 

  Usage: asa.py -s n_sphere in_pdb [out_pdb]
  
  - out_pdb    PDB file in which the atomic ASA values are written 
               to the b-factor column.
               
  -s n_sphere  number of points used in generating the spherical
               dot-density for the calculation (default=960). The 
               more points, the more accurate (but slower) the 
               calculation.

  """

  opts, args = getopt.getopt(sys.argv[1:], "n:")
  if len(args) < 1:
    print usage
    return
    
  mol = molecule.Molecule(args[0])
  atoms = mol.atoms()
  molecule.add_radii(atoms)

  n_sphere = 60
  for o, a in opts:
    if '-n' in o:
      n_sphere = int(a)
      print "Points on sphere: ", n_sphere
  asas = calculate_asa(atoms, 1.4, n_sphere)
  print "%.1f angstrom squared." % sum(asas)

  if len(args) > 1:
    for asa, atom in zip(asas, atoms):
      atom.bfactor = asa
    mol.write_pdb(args[1])
  
  
if __name__ == "__main__":
  main()


    
