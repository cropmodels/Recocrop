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
		
		size_t nsteps = 12;
		int duration=-1;
		bool max=false;
		
		std::vector<std::vector<double>> parameters;
		std::vector<std::string> parameter_names;
		std::vector<std::vector<double>> predictors;
		std::vector<std::string> predictor_names;
		std::vector<bool> dynamic;
		size_t vsize = 0;
		
		std::vector<std::string> messages;


		void predict_static(const size_t pari, const double& pred, std::vector<double> &x);
		void predict_dynamic(const size_t pari, const std::vector<double>& preds, std::vector<double> &x);


		std::vector<double> out;
		void run();
		bool predict(const int& iprd, const int& ipar, std::vector<double> &x);

		void setParameter(std::string name, std::vector<double> p);
		bool removeParameter(std::string name);

		void setPredictor(std::string name, std::vector<double> p, bool is_dynamic);
		bool removePredictor(std::string name);

	//	std::vector<double> output() {
	//		return std::vector<double> {data};
	//	}
	//	std::vector<double> runbatch(std::vector<std::vector<double>> predictors; std::vector<std::string> names);
};


