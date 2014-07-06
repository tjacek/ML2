#ifndef __NAIVE_WORKER__
#define __NAIVE_WORKER__

#include <map>
#include <algorithm>
#include <hash_map>
#include <string.h>
#include <string>
#include "erl_comm.h"
#include "mllib.h"


class NaiveWorker
{
	struct MapKeyNominal
	{
		int cat, attr, val;
		MapKeyNominal() {}
		MapKeyNominal(int c, int a, int v) : cat(c), attr(a), val(v) {}
		bool operator<(const MapKeyNominal &r) const
		{
			if (cat != r.cat)
				return cat < r.cat;
			if (attr != r.attr)
				return attr < r.attr;
			return val < r.val;
		}
	};
	
	struct MapKeyContinuous
	{
		int cat, attr;
		MapKeyContinuous() {}
		MapKeyContinuous(int c, int a) : cat(c), attr(a) {}
		bool operator<(const MapKeyContinuous &r) const
		{
			if (cat != r.cat)
				return cat < r.cat;
			return attr < r.attr;
		}
	};
	
	struct eqstr
	{
		bool operator()(const char* s1, const char* s2) const
		{
			return strcmp(s1, s2) == 0;
		}
	};

	typedef __gnu_cxx::hash_map<const char*, int, __gnu_cxx::hash<const char*>, eqstr> AtomHashMap;
	typedef std::pair<double, double> DoublePair;
	
private:
	FILE *logFile;
	
	std::map<MapKeyNominal,int> probNominalMap;
	std::map<MapKeyContinuous, DoublePair> probContinuousMap;
	std::map<int, int> aprioriProbMap;
	
	std::map<long,std::string> hashAtomMap;
	AtomHashMap atomHashMap;


	double getCPUTime();
	int *attributesToArray(ETERM *attributes);
	int getHash(const char *str);
	
	char *getAtomStr(ETERM *t);
	ETERM *makeTermFromStr(const char *str);
	
	ETERM *makeResultElemNominal(MapKeyNominal key, int value);
	ETERM *makeResultElemContinuous(MapKeyContinuous key, DoublePair value);
	ETERM *makeResultElemApriori(int key, int value);
	ETERM *makeResult();

public:
	NaiveWorker();
	NaiveWorker(const char *logPath);
	~NaiveWorker();

	void log(const char *msg);
	ETERM *computeConditional(ETERM *attributes, ETERM *tes);
};

#endif // __NAIVE_WORKER__
