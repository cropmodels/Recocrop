#include <vector>
#include <algorithm>
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
		return true;
	}
	return false;
};
		
void EcocropModel::setPredictor(std::string name, std::vector<double> p) {
	// check length of predictor. Should be 1 or nsteps (12 for now).
	int m = match(predictor_names, name);
	if (m > -1) {
		predictors[m] = p;
	} else {
		predictor_names.push_back(name);
		predictors.push_back(p);
	}
};


bool EcocropModel::removePredictor(std::string name) {
	int m = match(predictor_names, name);
	if (m > -1) {
		predictors.erase(predictors.begin()+m);
		return true;
	}
	return false;
};


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


bool predict(std::vector<double> &output, const std::vector<double> &pred, const std::vector<double> &pars) {
	if (pred.size() == 12) {
		std::vector<double> result(12);
		for (size_t i=0; i<12; i++) {
			output[i] = std::min(output[i], approx4(pars, pred[i]));
		}
	} else {
		double app = approx4(pars, pred[0]);
		for (size_t i=0; i<12; i++) {
			output[i] = std::min(output[i], app);
		}
	}
	return true;
}


void EcocropModel::run() {
	out = std::vector<double>(12, 1);	
	messages.resize(0);
	hasError = false;
	if (duration < 0) {
		std::string txt = "duration must be >= 0"; 
		messages.push_back(txt);
		hasError = true;
		return;
	}
	for (size_t i=0; i<predictors.size() ; i++) {
		int m = match(parameter_names, predictor_names[i]); 
		if (m == -1) {
			for (double &d  : out ) d = NAN; 
			std::string txt = "no parameters for " + predictor_names[i]; 
			messages.push_back(txt);
			hasError = true;
			break;
		} else {
			predict(out, predictors[i], parameters[m]);
		}		
	}	
}

	
/*

std::vector<double> EcocropModel::runbatch(std::vector<double> Ns, std::vector<double> Ps, std::vector<double> Ks, std::vector<double> Ya) {
	
	size_t n = Ns.size();
	std::vector<double> out(n, NAN); 
	for (size_t i=0; i<n; i++) {
		if (isnan(Ns[i])) continue;	
		stem_att = 0.55 * store_att;
		run();
		out[i] = store_lim;
	}
	return out;
}	
*/



