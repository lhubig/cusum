#include <Rcpp.h>
#include <random>
#include <tuple>
#include "quantiles.h"

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

//' @name gcusum
//' @title Grouped-CUSUM chart
//' @description Calculate GCUSUM chart for non-risk-adjusted processes.
//' 
//' @param input_outcomes NumericMatrix, patient outcomes and block_id (continuous).
//' @param failure_prob double, accepted failure probability of null hypothesis.
//' @param delta double, odds multiplier.
//' @param control_limit double, control limit.
//' @param max_num_shuffles integer, number of shuffles.
//' @param seed integer.
//' @param quantiles NumericVector, quantiles that are returned.
//' @return gcusum NumericMatix, signal probability, average CUSUM value and specified quantiles for every observation.
//' @export
// [[Rcpp::export(gcusum)]]
NumericMatrix gcusum(NumericMatrix& input_outcomes,
                     double failure_prob, 
                     double delta,
                     double control_limit, 
                     int max_num_shuffles,
                     int seed,
                     NumericVector& quantiles) {
  /*
   * Calculate GCUSUM for non-risk-adjusted processes
   *  
   * input_outcomes: first column outcomes, second column block id (continuous)
   * failure_prob: baseline failure probability
   * delta: detection level, odds multiplier for alternative hyptohesis
   * control_limit: control limit (estimated by cusum::cusum_limit_sim)
   * max_num_shuffles: number of shuffles for observation groups
   * seed: for RNG
   */
  
  // calculate alternative probability of failure
  double odds_A = delta * failure_prob/(1-failure_prob);;
  double prob_A = odds_A/(1+odds_A);
  
  //calculate CUSUM weights
  double weight_f =  std::round(65536. * std::log(prob_A/failure_prob)) / 65536;
  double weight_s = std::round(65536. * std::log((1-prob_A)/(1-failure_prob))) / 65536;
  
  assert(input_outcomes.size() > 0);
  assert(input_outcomes(0,1) == 1);
  
  std::mt19937_64 generator(seed);
  
  // collect outcomes in boolean vector  
  std::vector<bool> outcomes(input_outcomes.nrow());
  for(int i = 0; i != input_outcomes.nrow(); ++i) {
    outcomes[i] = input_outcomes(i,0);
  }
  
  // define start and end id of blocks
  std::vector<std::pair<std::size_t, std::size_t> > block_boundaries;
  { block_boundaries.reserve(input_outcomes(input_outcomes.nrow()-1, 1));
    std::pair<std::size_t, std::size_t> working_element(0,0);
    int current_id = 1;
    for(int i = 1; i != input_outcomes.nrow(); ++i) {
      if (input_outcomes(i, 1) != current_id) {
        working_element.second = i;
        block_boundaries.push_back(working_element);
        working_element.first = i;
        assert(current_id + 1 = input_outcomes(i,1));
        current_id = input_outcomes(i,1);
      }
    }
    working_element.second = input_outcomes.nrow();
    block_boundaries.push_back(working_element);
  }
  
  // create empty storage container for ultimative results ////////////////////
  NumericMatrix returns(input_outcomes.nrow(), 2 + quantiles.size());
  
  std::unordered_map<double, double> start_values;
  start_values[0] = 1.;
  
  //iterate through blocks based on id, shuffles outcomes and calculates the cusum
  for(auto const& block : block_boundaries) {
    // create empty storage containter for cusum_statistics and start_value probability ////////////////////
    std::unordered_map<double, double> new_start_values;
    std::size_t num_shuffles = max_num_shuffles;
    
    // consider adapting num_shuffles
    if (block.second - block.first == 1) {
      num_shuffles = 1;
    }/* else if (block.second - block.first > 1) {
   unsigned long long int factorial = 1;
   
   for(unsigned int count = 1; count <= block.second - block.first; ++count) {
   factorial *= count;              // factorial=factorial*count
   }
   num_shuffles = std::min(num_shuffles, factorial);
    }*/
  
  std::vector<std::vector<std::pair<double, double> > > block_cusum_values(block.second - block.first);
    for(auto& elem : block_cusum_values) {
      elem.reserve(num_shuffles * start_values.size());
    }
    
    double increase = 1./num_shuffles;
    for(auto const& start_value : start_values) {
      double scaled_increase = increase * start_value.second;
      for(std::size_t iter = 0; iter != num_shuffles; ++iter) {
        std::shuffle(outcomes.begin() + block.first, outcomes.begin() + block.second, generator);
        double cs = start_value.first;
        for(std::size_t i = block.first; i != block.second; ++i) {
          cs = std::max(0., cs + (outcomes[i] ? weight_f : weight_s));
          if (cs >= control_limit) {
            returns(i,0) += scaled_increase;
          }
          returns(i,1) += cs * scaled_increase;
          block_cusum_values[i-block.first].push_back(std::make_pair(cs, start_value.second));
        }
        new_start_values[cs] += start_value.second;
      }
    }
    
    // select final states of cusum
    double sum_end_values = 0.;
    for(auto const& p : new_start_values) {
      sum_end_values += p.second;
    }
    start_values.clear();
    
    for(auto& p : new_start_values) {
      start_values[std::round(65536. * p.first) / 65536] += p.second / sum_end_values;
    }
    
    // calculate results and store ////////////////////
    for(std::size_t t_offset = 0; t_offset != block_cusum_values.size(); ++t_offset) {
      auto q = quantile(block_cusum_values[t_offset], quantiles);
      for(int qi = 0; qi != q.size(); ++qi) {
        returns(t_offset + block.first, qi + 2) = q(qi);
      }
    }
  }
  
  return returns;
}


//' @name ragcusum
//' @title RA-Grouped-CUSUM chart
//' 
//' @description Calculate GCUSUM chart for risk-adjusted processes.
//' 
//' @param input_ra_outcomes NumericMatrix, patient outcomes and block_id (continuous).
//' @param control_limit double, control limit.
//' @param max_num_shuffles integer, number of shuffles.
//' @param seed integer.
//' @param quantiles NumericVector, quantiles that are returned.
//' @return ragcusum NumericMatix, signal probability, average CUSUM value and specified quantiles for every observation.
//'  
//' @export
// [[Rcpp::export(ragcusum)]]
NumericMatrix ragcusum(NumericMatrix& input_ra_outcomes,
                       double control_limit, 
                       int max_num_shuffles,
                       int seed,
                       NumericVector& quantiles) {
  /*
   * Calculate GCUSUM for risk-adjusted processes. 
   * 
   * input_ra_outcomes: 
   *     0: outcomes in 0 and 1
   *     1: weight for failure
   *     2: weight for success
   *     3: block id
   * control_limit: simulate by cusum::racusum_limit_sim() or better
   * max_num_shuffle: maximum number of shuffles of one data block (can be smaller for n_b <= 5)
   * seed: for generator
   */
  assert(ra_outcomes.size() > 0);
  assert(ra_outcomes(0,3) == 1);
  
  std::mt19937_64 generator(seed);
  
  // connect outcomes and weights in tuple
  std::vector<std::tuple<bool, double, double> > ra_outcomes(input_ra_outcomes.nrow());
  for(int i = 0; i != input_ra_outcomes.nrow(); ++i) {
    std::get<0>(ra_outcomes[i]) = input_ra_outcomes(i,0); 
    std::get<1>(ra_outcomes[i]) = input_ra_outcomes(i,1);
    std::get<2>(ra_outcomes[i]) = input_ra_outcomes(i,2);
  }
  
  // define start and end id of blocks
  std::vector<std::pair<std::size_t, std::size_t> > block_boundaries;
  { block_boundaries.reserve(input_ra_outcomes(input_ra_outcomes.nrow()-1, 3));       
    std::pair<std::size_t, std::size_t> working_element(0,0);
    int current_id = 1;
    for(int i = 1; i != input_ra_outcomes.nrow(); ++i) {
      if (input_ra_outcomes(i, 3) != current_id) {
        working_element.second = i;
        block_boundaries.push_back(working_element);
        working_element.first = i;
        assert(current_id + 1 = input_ra_outcomes(i,3));
        current_id = input_ra_outcomes(i,3);
      }
    }
    working_element.second = input_ra_outcomes.nrow();
    block_boundaries.push_back(working_element);
  }
  
  NumericMatrix returns(input_ra_outcomes.nrow(), 2 + quantiles.size());
  
  std::unordered_map<double, double> start_values;
  start_values[0] = 1.;                                                                    
  
  // iterate over blocks
  for(auto const& block : block_boundaries) {
    //std::cerr << "Working on block (" << block.first << "," << block.second << ")\n";
    
    std::size_t num_shuffles = max_num_shuffles;
    // consider adapting num_shuffles
    if (block.second - block.first == 1) {
      num_shuffles = 1;
    } 
    
    std::vector<std::vector<std::pair<double, double> > > block_cusum_values(block.second - block.first);
    for(auto& elem : block_cusum_values) {
      elem.reserve(num_shuffles * start_values.size());
    }
    
    double increase = 1./num_shuffles;
    
    std::unordered_map<double, double> new_start_values;
    
    for(auto const& start_value : start_values) {
      double scaled_increase = increase * start_value.second;
      for(std::size_t iter = 0; iter != num_shuffles; ++iter) {
        std::shuffle(ra_outcomes.begin() + block.first, ra_outcomes.begin() + block.second, generator);
        double cs = start_value.first;
        for(std::size_t i = block.first; i != block.second; ++i) {
          cs = std::max(0., cs + ( std::get<0>(ra_outcomes[i]) ? std::get<1>(ra_outcomes[i]) : std::get<2>(ra_outcomes[i])));
          if (cs >= control_limit) {
            returns(i,0) += scaled_increase;
          }
          returns(i,1) += cs * scaled_increase;
          block_cusum_values[i-block.first].push_back(std::make_pair(cs, start_value.second));
        }
        new_start_values[cs] += start_value.second;
      }
    }
    
    double sum_end_values = 0.;
    for(auto const& p : new_start_values) {
      sum_end_values += p.second;
    }
    start_values.clear();
    
    for(auto& p : new_start_values) {
      start_values[std::round(8192. * p.first) / 8192] += p.second / sum_end_values;
    }
    
    for(std::size_t t_offset = 0; t_offset != block_cusum_values.size(); ++t_offset){
      auto q = quantile(block_cusum_values[t_offset], quantiles);
      for(int qi = 0; qi != q.size(); ++qi) {
        returns(t_offset + block.first, qi + 2) = q(qi);
      }
    }
  }
  
  return returns;
}
