#ifndef _ISING_H_
#define _ISING_H_

#include <iostream>
#include <fstream>
#include "ising_system.hpp"

using std::fstream;

void MC_simulation(int Nside, int Niterations, double T, fstream &fout);

#endif //_ISING_H_
