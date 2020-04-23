#include <Rcpp.h>
#include "ecocrop.h"

using namespace Rcpp;


RCPP_EXPOSED_CLASS(EcocropModel)
	
RCPP_MODULE(ECOCROP){

    class_<EcocropModel>("EcocropModel")
		.constructor()
		.method("setParameter", &EcocropModel::setParameter, "setParameter")
		.method("setPredictor", &EcocropModel::setPredictor, "setPredictor")
		.method("removeParameter", &EcocropModel::removeParameter, "removeParameter")
		.method("removePredictor", &EcocropModel::removePredictor, "removePredictor")
		.method("run", &EcocropModel::run, "run") 
		.method("names", &EcocropModel::names, "names of output variables") 
		.method("setOptions",  &EcocropModel::setOptions, "set output options") 

		.property("out", &EcocropModel::get_out, &EcocropModel::set_out, "get model output or reset it") 
		.property("is_sum", &EcocropModel::get_is_sum, &EcocropModel::set_is_sum, "sum or average?" )
		
		.field("nyears",  &EcocropModel::nyears, "nyears")
		.field("duration",  &EcocropModel::duration, "duration")
		.field("get_max",   &EcocropModel::get_max, "get_max")
		.field("which_max", &EcocropModel::which_max, "which_max")
		.field("count_max", &EcocropModel::count_max, "count_max")
		.field("lim_fact",  &EcocropModel::lim_fact, "lim_fact")
				
		.field_readonly("parameters", &EcocropModel::parameters, "parameters")
		.field_readonly("parameter_names", &EcocropModel::parameter_names, "parameter_names")
		.field_readonly("predictors", &EcocropModel::predictors, "predictors")
		.field_readonly("predictor_names", &EcocropModel::predictor_names, "predictor_names")
		.field("hasError", &EcocropModel::hasError, "hasError")
		.field("messages", &EcocropModel::messages, "messages")
	;			
}

