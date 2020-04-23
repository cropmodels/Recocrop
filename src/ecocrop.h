/*
 Author: Robert Hijmans
 Date: March 2020
 License: GPL (>=2)
*/


class EcocropModel {

	public:
		EcocropModel();
		std::string name;
		bool hasError;
		
		size_t nyears  = 1;
		size_t nsteps  = 24;
		int duration   = -1;
		bool get_max   = false;
		bool which_max = false;
		bool count_max = false;
		bool lim_fact  = false;
		
		std::vector<std::vector<double>> parameters;
		std::vector<std::vector<double>> pred_pars;
		std::vector<std::string> parameter_names;
		std::vector<std::vector<double>> predictors;
		std::vector<std::string> predictor_names;
		std::vector<bool> is_sum;
		std::vector<bool> dynamic;
		size_t vsize = 0;
		
		std::vector<std::string> messages;

		bool predict_static(const size_t pari, const double& pred, std::vector<double> &x, std::vector<double> &mf);
		bool predict_dynamic(const size_t pari, const std::vector<double>& preds, std::vector<double> &x, std::vector<double> &mf);

		std::vector<double> out;
		std::vector<std::string> names();
		
		void run();
		bool predict(const int& iprd, const int& ipar, std::vector<double> &x);

		void setParameter(std::string name, std::vector<double> p);
		bool removeParameter(std::string name);

		void setPredictor(std::string name, std::vector<double> p, bool is_dynamic);
		bool removePredictor(std::string name);

		void setOptions(bool _get_max, bool _which_max, bool _count_max, bool _lim_fact);
		void movingmin_circular(std::vector<double>& v, int &window);
		
		std::vector<bool> get_is_sum() {return is_sum;};
		void set_is_sum(std::vector<bool> x) {
			if (x.size() == is_sum.size()) {
				is_sum = x;
			}
		}
	
		std::vector<double> get_out() { return out; };
		void set_out(std::vector<double> x) { out.resize(0); };
		
};

