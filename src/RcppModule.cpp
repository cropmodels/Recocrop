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
//		.method("runbatch", &EcocropModel::runbatch, "run the model")	
		.field("duration", &EcocropModel::duration, "duration")
		.field("max", &EcocropModel::max, "max")
		.field_readonly("parameters", &EcocropModel::parameters, "parameters")
		.field_readonly("parameter_names", &EcocropModel::parameter_names, "parameter_names")
		.field_readonly("predictors", &EcocropModel::predictors, "predictors")
		.field_readonly("predictor_names", &EcocropModel::predictor_names, "predictor_names")
		.field_readonly("out", &EcocropModel::out, "out")
		.field("hasError", &EcocropModel::hasError, "hasError")
		.field("messages", &EcocropModel::messages, "messages")
	;			
}
