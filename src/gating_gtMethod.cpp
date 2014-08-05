/*
 * gating_gtMethod.cpp
 *
 *  Created on: Aug 4, 2014
 *      Author: wjiang2
 */

#include <Rcpp.h>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/lexical_cast.hpp>
// [[Rcpp::export]]
bool gating_gtMethod(Rcpp::S4 x, Rcpp::S4 y, Rcpp::S4 gtPop, Rcpp::String parent, Rcpp::List pp_res){

	//parse arguments of gtmethod object
	Rcpp::List args = x.slot("args");
//   HOTFIX: This resolve an error when args is a named list with name NA and object NA.
//   The resulting error occurs down below and is:
//   Error in thisCall[[arg]] <- args[[arg]] : subscript out of bounds
//  if (!is.null(names(args))) {
//    args <- args[!is.na(names(args))]
//  }

	//parse gtmethod name
	std::string gm = x.slot("name");
	gm = "." + gm;
	//parse dimensions
	std::string strDims = x.slot("dims");
	std::vector<std::string> dims;
	boost::split(dims, strDims, boost::is_any_of(","));
	bool is_1d_gate = dims.size() == 1;
	//parse gt population object
	std::string popAlias = gtPop.slot("alias");
//	Rcpp::Environment openCyto("package::openCyto");

	std::string popName = gtPop.slot("name");

	//get children nodes
//	GatingSet *	gs=getGsPtr(_gsPtr);
//	bool showHidden = as<bool>(_showHidden);
//	string sampleName=as<string>(_sampleName);
//	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
//	string gatePath=as<string>(_gatePath);
//	NODEID u = gh->getNodeID(gatePath);
//	VertexID_vec childrenID = gh->getChildren(u);
//	vector<NODEID> res;
//	for(VertexID_vec::iterator it=childrenID.begin(); it!=childrenID.end();it++){
//		NODEID thisNodeID = *it;
//		bool isHidden = gh->getNodeProperty(thisNodeID).getHiddenFlag();
//		if(showHidden||(!isHidden))
//			res.push_back(thisNodeID);
//	}

//	gs_nodes <- basename(getChildren(y[[1]], parent))
	Rcpp::Environment global = Rcpp::Environment::global_env();
	Rcpp::Function getChildren= global[".getChildren"];
	Rcpp::StringVector gs_nodes = getChildren(y, parent);
	bool isGated = false;
	for(unsigned i = 0; i < gs_nodes.size(); i++){
		std::string thisNode = Rcpp::as<std::string>(gs_nodes(i));
		if(thisNode.compare(popAlias) == 0){
			isGated = true;
			break;
		}

	}
	if(isGated){
		Rcpp::Rcout << "Skip gating! Population '" << popAlias << "' already exists.";
		return Rcpp::List();
	}else{
		Rcpp::Rcout << "Gating for '" << popAlias << "'";

	    std::string groupBy = x.slot("groupBy");
		bool isCollapse = x.slot("isCollapse");

		Rcpp::Function getData= global[".getData"];
		Rcpp::S4 parent_data = getData(y, parent);


		Rcpp::StringVector split_by;
		if(groupBy.size() > 0 && isCollapse){
//			when x@collapse == FALSE,then ignore groupBy argument since grouping is only used for collapsed gating
			try{
				unsigned split_by_num = boost::lexical_cast<unsigned>(groupBy);

			}
			catch(int e){
				//split by study variables
				boost::split(split_by, groupBy, boost::is_any_of(":"));
				Rcpp::Function splitBy= global[".splitBy"];
				split_by = splitBy(parent_data, split_by);

				}

			//split by every N samples
			Rcpp::Function length= global[".length"];
		  unsigned nSamples = length(parent_data);
		  if(nSamples == 1){
			split_by = "1";
		  }else{
			split_by =  sample(rep(1:nSamples, each = split_by_num, length.out= nSamples))
		  }


		}else{
			Rcpp::Function sampleNames= global[".sampleNames"];
			split_by = sampleNames(parent_data);
		}

//		frm <- parent_data[[1, use.exprs = FALSE]]
//	    channels <- sapply(dims, function(channel)as.character(getChannelMarker(frm, channel)$name))
//	    parent_data <- parent_data[, channels] #it is more efficient to only pass the channels of interest
		Rcpp::Function split= global[".split"];
		fslist = split(parent_data, split_by);
	}
	return isGated;
}

