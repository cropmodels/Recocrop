#include <Rcpp.h>
#include "ecocrop.h"

using namespace Rcpp;


RCPP_EXPOSED_CLASS(EcocropModel)
	
RCPP_MODULE(ECOCROP){

    class_<EcocropModel>("EcocropModel")
		.constructor()
		.method("setParameter", &EcocropModel::setParameter, "setParameter")
		.method("setPredictor", &EcocropModel::setPredictor, "setPredictor")
		.method("run", &EcocropModel::run, "run") 
//		.method("runbatch", &EcocropModel::runbatch, "run the model")	
		.field("duration", &EcocropModel::duration, "duration")
		.field("parameters", &EcocropModel::parameters, "parameters")
		.field("parameter_names", &EcocropModel::parameter_names, "parameter_names")
		.field("predictors", &EcocropModel::predictors, "predictors")
		.field("predictor_names", &EcocropModel::predictor_names, "predictor_names")
		.field("out", &EcocropModel::out, "out")
		.field("hasError", &EcocropModel::hasError, "hasError")
		.field("messages", &EcocropModel::messages, "messages")
	;			
}
