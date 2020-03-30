/*
Author: Robert Hijmans
Date: March 2020
License: GPL (>=3)
*/


class EcocropModel {
	public:
		EcocropModel();
		std::string name;
		bool hasError;

		double duration=-1;
	
		std::vector<std::vector<double>> parameters;
		std::vector<std::string> parameter_names;
		std::vector<std::vector<double>> predictors;
		std::vector<std::string> predictor_names;
		
		std::vector<std::string> messages;

		std::vector<double> out;
		void run();

		void setParameter(std::string name, std::vector<double> p);
		bool removeParameter(std::string name);

		void setPredictor(std::string name, std::vector<double> p);
		bool removePredictor(std::string name);


	//	std::vector<double> output() {
	//		return std::vector<double> {data};
	//	}
	//	std::vector<double> runbatch(std::vector<std::vector<double>> predictors; std::vector<std::string> names);
};


