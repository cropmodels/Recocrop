// Copyright (c) 2020  Robert J. Hijmans
//
// This file is part of the "ecocrop" software.
//
// spat is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// spat is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// A copy of the GNU General Public License
// is available here <http://www.gnu.org/licenses/>.

#include <vector>
#include <algorithm>
#include <string>
#include <cmath>
#include "ecocrop.h"


EcocropModel::EcocropModel() {
	hasError = false;
}

template < typename T>
int match(const std::vector<T>  &v, const T &value) {
	int result = -1;
	auto it = std::find(v.begin(), v.end(), value);
	if (it != v.end()) {
		result = distance(v.begin(), it);
	} 
	return result;
}


void EcocropModel::setParameter(std::string name, std::vector<double> p) {
	// need to first check that e.name is not empty
	// and some more sanity checks as well
	int m = match(parameter_names, name);
	if (m > -1) {
		parameters[m] = p;
	} else {
		parameter_names.push_back(name);
		parameters.push_back(p);
	}
};

bool EcocropModel::removeParameter(std::string name) {
	int m = match(parameter_names, name);
	if (m > -1) {
		parameters.erase(parameters.begin()+m);
		parameter_names.erase(parameter_names.begin()+m);
		return true;
	} else if (name == "ALL") {
		parameters.resize(0);
		parameter_names.resize(0);
		return true;
	}
	return false;
};
		
void EcocropModel::setPredictor(std::string name, std::vector<double> p, bool is_dynamic) {
	size_t np = p.size();
	if (is_dynamic) {
		if (!( (np > 0) & ((np % nsteps) == 0))) {
			hasError = true;
			std::string txt = "length of " + name + " should be divisible by " + std::to_string(nsteps);
			messages.push_back(txt);				
		}
	} 
	if (vsize == 0) {
		vsize = is_dynamic ? np / nsteps : np;
	} 
	
	size_t vvsize = is_dynamic ? vsize * nsteps : vsize;
	if (vvsize != np) {
		hasError = true;
		std::string txt = "length of " + name + " should be " + std::to_string(vvsize);
		messages.push_back(txt);
	} else {
		int m = match(predictor_names, name);
		if (m > -1) {
			predictors[m] = p;
			dynamic[m] = is_dynamic;
		} else {
			predictor_names.push_back(name);
			predictors.push_back(p);
			dynamic.push_back(is_dynamic);
		}
	}
};


bool EcocropModel::removePredictor(std::string name) {
	int m = match(predictor_names, name);
	if (m > -1) {
		predictors.erase(predictors.begin()+m);
		predictor_names.erase(predictor_names.begin()+m);
		dynamic.erase(dynamic.begin()+m);
		if (predictors.size() == 0) vsize = 0;
		return true;
	} else if (name == "ALL") {
		predictors.resize(0);
		predictor_names.resize(0);
		dynamic.resize(0);
		vsize = 0;
		return true;
	}
	return false;
};



void EcocropModel::setOptions(bool _get_max, bool _which_max, bool _count_max, bool _lim_fact) {
	get_max   = _get_max;
	which_max = _which_max;
	count_max = _count_max;
	lim_fact  = _lim_fact;
}



std::vector<std::string> EcocropModel::names(){
	std::vector<std::string> s;
	unsigned ns = (count_max + get_max + which_max);
	if (lim_fact) {
		s = {"m_jan", "m_feb", "m_mar", "m_apr", "m_may", "m_jun", "m_jul", "m_aug", "m_sep", "m_oct", "m_nov", "m_dec"};
	} else if (ns > 0) {
		if (get_max)   s.push_back("max");
		if (which_max) s.push_back("which_max");
		if (count_max) s.push_back("count_max");
	} else {
		s = {"jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"};
	}
	return s;
}

double approx4(const std::vector<double> &v, const double &x) {
	double result;
	if (x < v[0] || x > v[3]) {
		result = 0;
	} else if (x >= v[1] && x <= v[2]) {
		result = 1;
	} else if (x >= v[2]) {
		result = ((v[3] - x) / (v[3] - v[2]));
	} else {
		result = ((x - v[0]) / (v[1] - v[0]));
	}
	return(result);
}	


template <typename T>
void movingmin_circular(std::vector<T>& v, int &window) {
	unsigned nmax = v.size();
	// window must be < nmax
	v.insert(v.end(), v.begin(), v.end());
	for (size_t i=0; i<nmax; i++) {
		for (size_t j=i; j<(i+window); j++) {
			v[i] = v[i] < v[j] ? v[i] : v[j];
		}
	}
	v.erase(v.begin()+nmax, v.end());
}


/*
std::vector<double> pred12to24(std::vector<double> &p, const bool &prec){
	std::vector<double> out(24);
	if (prec) {
		for (size_t i=0; i<12; i++) {
			p[i] = p[0] / 2;
		}
	}
	out[0] = (p[0] + p[12])/2;
	out[1] = p[0];
	for (size_t i=1; i<12; i++) {
		size_t j = i * 2;
		out[j] = (p[i] + p[i-1])/2;
		out[j+1] = p[i];
	}
	return out;
}
*/


bool EcocropModel::predict_dynamic(const size_t pari, const std::vector<double>& preds, std::vector<double> &x, std::vector<double> &mf) {
	for (size_t i=0; i<nsteps; i++) {
		if (std::isnan(preds[i])) {
			for (size_t i=0; i<nsteps; i++) {
				x[i] = NAN;
			}
			return false;
		} else {
			for (size_t i=0; i<nsteps; i++) {
				double app = approx4(parameters[pari], preds[i]);
				if (lim_fact) {
					if (app < x[i]) {
						x[i] = app;
						mf[i] = pari+1;
					}
				} else {
					x[i] = std::min(x[i], app);
				}
			}
		}
	}
	return true;
}

bool EcocropModel::predict_static(const size_t pari, const double& pred, std::vector<double> &x, std::vector<double> &mf) {
	if (std::isnan(pred)) {
		for (size_t i=0; i<nsteps; i++) {
			x[i] = NAN;
		}
		return false;
	} else {
		double app = approx4(parameters[pari], pred);
		if (lim_fact) {
			for (size_t i=0; i<nsteps; i++) {
				if (app < x[i]) {
					x[i] = app;
					mf[i] = pari+1;
				}
			}			
		} else {
			for (size_t i=0; i<nsteps; i++) {
				x[i] = std::min(x[i], app);
			}
		}
	}
	return true;	
}


void EcocropModel::run() {

	out = std::vector<double>(0);	
	messages.resize(0);
	//hasError = false;
	if (hasError) return;
	
	if (duration < 0 || duration > int(nsteps)) {
		std::string txt = "duration must be between 1 and " + std::to_string(nsteps);; 
		messages.push_back(txt);
		hasError = true;
		return;
	}
	
	std::vector<unsigned> p(predictors.size());
	for (size_t j=0; j<predictors.size() ; j++) {
		int m = match(parameter_names, predictor_names[j]); 
		if (m == -1) {
			std::string txt = "no parameters for " + predictor_names[j]; 
			messages.push_back(txt);
			hasError = true;
			return;
		} else {
			p[j] = m;
		}
	}


/*
	std::vector<bool> is_prec(predictors.size(), false);
	std::vector<std::vector<double>> matched_parameters(predictors.size());	
	for (size_t i=0; i<predictors.size(); i++) {
		int m = match(parameter_names, predictor_names[i]); 
		if (m == -1) {
			std::string txt = "no parameters for " + predictor_names[i]; 
			messages.push_back(txt);
			hasError = true;
			return;
		} else {
			matched_parameters[i] = parameters[m];
			if (predictor_names[i] == "prec") {
				is_prec[i] = true;
				for (size_t j=0; j<4; i++) matched_parameters[i][j] = matched_parameters[i][j] / 2;
			}
		}
	}

*/
	size_t nsummary = 0;
	bool summary = false;
	if (!lim_fact) {
		nsummary = count_max + get_max + which_max;
		summary = nsummary > 0;
	} 
	
	std::vector<double> sumnan(nsummary, NAN);
	unsigned n = (nsummary > 0) ? (nsummary * vsize) : (nsteps * vsize);
	out.reserve(n);

	bool success = true;
	std::vector<double> x(nsteps, 1);
	std::vector<double> mf(nsteps, 0);

	for (size_t i=0; i<vsize; i++) {
		
		std::fill(x.begin(), x.end(), 1);
		if (lim_fact) {
			std::fill(mf.begin(), mf.end(), 0);
		}
		
		size_t dstart = i * nsteps;
		size_t dend = dstart + nsteps;
		for (size_t j=0; j<predictors.size() ; j++) {
			if (dynamic[j]) {
				std::vector<double> preds(predictors[j].begin()+dstart, predictors[j].begin()+dend);
				success = predict_dynamic(p[j], preds, x, mf);
			} else {
				double pred = predictors[j][i];				
				success = predict_static(p[j], pred, x, mf);
			}
			if (!success) break;
		}
		
		if (success) {
			if (lim_fact) {	
				out.insert(out.end(), mf.begin(), mf.end());
			} else {
				movingmin_circular(x, duration); 
				if (summary) {
					std::vector<double>::iterator it = std::max_element(x.begin(), x.end());
					double maxv = *it;
					if (get_max) {
						out.push_back(maxv);
					}
					if (which_max) {
						double wmax = NAN;
						if (maxv > 0) {
							wmax = 1 + std::distance(x.begin(), it);
						}
						out.push_back(wmax);
					}
					if (count_max) {
						double mcount = NAN;
						if (maxv > 0) {
							mcount = std::count(x.begin(), x.end(), maxv);
						}
						out.push_back(mcount);
					}
				} else {
					out.insert(out.end(), x.begin(), x.end());
				}
			}
		} else { // NA
			if (summary) {
				out.insert(out.end(), sumnan.begin(), sumnan.end());
			} else {
				out.insert(out.end(), x.begin(), x.end()); // NA - same for lim_fact
			} 
		}
	}
}

