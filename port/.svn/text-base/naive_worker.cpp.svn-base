#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <sys/resource.h>
#include "naive_worker.h"

using namespace std;


NaiveWorker::NaiveWorker()
	: logFile(NULL)
{
}


NaiveWorker::NaiveWorker(const char *logPath)
{
	logFile = fopen(logPath, "a");
	time_t logTime = time(NULL);
	tm *logDatetime = localtime(&logTime);
	fprintf(logFile, "\n-----------------------------------------------\
		\n   Worker log - %s-----------------------------------------------\n\n",
		asctime(logDatetime));
}


NaiveWorker::~NaiveWorker()
{
	if (logFile)
		fclose(logFile);
}


double NaiveWorker::getCPUTime()
{
	timeval tim;
	rusage ru;
	getrusage(RUSAGE_SELF, &ru);
	tim = ru.ru_utime;
	double t = (double)tim.tv_sec + (double)tim.tv_usec / 1000000.0;
	tim = ru.ru_stime;
	t += (double)tim.tv_sec + (double)tim.tv_usec / 1000000.0;
	return t;
}


void NaiveWorker::log(const char *msg)
{
	if (logFile == NULL)
		return;
	fprintf(logFile, "[%0.3f]  ", getCPUTime());
	fprintf(logFile, "%s\n", msg);
	fflush(logFile);
}

int *NaiveWorker::attributesToArray(ETERM *attributes)
{
	int numAttr = erl_length(attributes);
	int *attr = new int[numAttr];
	
	for (int i = 0; i < numAttr; i++)
	{
		attr[i] = ERL_INT_VALUE(erl_hd(attributes));
		attributes = erl_tl(attributes);
	}
	
	return attr;
}


int NaiveWorker::getHash(const char *str)
{
	AtomHashMap::iterator it = atomHashMap.find(str);
	
	if (it == atomHashMap.end())
	{
		//log("new hash");
		int h = atomHashMap.size();
		atomHashMap[str] = h;
		hashAtomMap[h] = str;
		return h;
	}
	else
	{
		//log("existing hash");
		return it->second;
	}
	
	return -1;
}


char *NaiveWorker::getAtomStr(ETERM *t)
{
	if (ERL_IS_INTEGER(t))
	{
		int v = ERL_INT_VALUE(t);
		char *ret = new char[abs(v)/10 + 2];
		sprintf(ret, "%d", v);
		return ret;
	}
	
	return ERL_ATOM_PTR(t);
}


ETERM *NaiveWorker::makeTermFromStr(const char *str)
{
	int val;
	if (sscanf(str, "%d", &val) == 1)
		return erl_mk_int(val);
	return erl_mk_atom(str);
}


ETERM *NaiveWorker::makeResultElemNominal(MapKeyNominal key, int value)
{
	string categoryStr = hashAtomMap[key.cat];
	string valueStr = hashAtomMap[key.val];
	int attrib = key.attr;
	int sum = value;
	
	ETERM* keyTupleArr[3];
	keyTupleArr[0] = makeTermFromStr(categoryStr.c_str());
	keyTupleArr[1] = erl_mk_int(attrib);
	keyTupleArr[2] = makeTermFromStr(valueStr.c_str());
	ETERM *keyTuple = erl_mk_tuple(keyTupleArr, 3);
	
	ETERM *elementArr[2];
	elementArr[0] = keyTuple;
	elementArr[1] = erl_mk_int(sum);
	
	//fprintf(logFile, "%s, %d, %s -> %d\n", categoryStr.c_str(), attrib, valueStr.c_str(), sum);
	
	return erl_mk_tuple(elementArr, 2);
}


ETERM *NaiveWorker::makeResultElemContinuous(MapKeyContinuous key, DoublePair value)
{
	string categoryStr = hashAtomMap[key.cat];
	int attrib = key.attr;
	
	ETERM* keyTupleArr[2];
	keyTupleArr[0] = makeTermFromStr(categoryStr.c_str());
	keyTupleArr[1] = erl_mk_int(attrib);
	ETERM *keyTuple = erl_mk_tuple(keyTupleArr, 2);
	
	ETERM* valueTupleArr[2];
	valueTupleArr[0] = erl_mk_float(value.first);
	valueTupleArr[1] = erl_mk_float(value.second);
	ETERM *valueTuple = erl_mk_tuple(valueTupleArr, 2);
	
	ETERM *elementArr[2];
	elementArr[0] = keyTuple;
	elementArr[1] = valueTuple;
	
	return erl_mk_tuple(elementArr, 2);
}


ETERM *NaiveWorker::makeResultElemApriori(int key, int value)
{
	string categoryStr = hashAtomMap[key];
	
	ETERM *elementArr[2];
	elementArr[0] = makeTermFromStr(categoryStr.c_str());
	elementArr[1] = erl_mk_int(value);
	
	return erl_mk_tuple(elementArr, 2);
}


ETERM *NaiveWorker::makeResult()
{
	ETERM *resultNominal = erl_mk_empty_list();
	ETERM *resultContinuous = erl_mk_empty_list();
	ETERM *resultApriori = erl_mk_empty_list();
	
	std::map<MapKeyNominal,int>::iterator itN;
	for (itN = probNominalMap.begin(); itN != probNominalMap.end(); itN++)
	{
		ETERM *element = makeResultElemNominal(itN->first, itN->second);
		resultNominal = erl_cons(element, resultNominal);
	}
	
	std::map<MapKeyContinuous,DoublePair>::iterator itC;
	for (itC = probContinuousMap.begin(); itC != probContinuousMap.end(); itC++)
	{
		ETERM *element = makeResultElemContinuous(itC->first, itC->second);
		resultContinuous = erl_cons(element, resultContinuous);
	}
	
	std::map<int,int>::iterator itA;
	for (itA = aprioriProbMap.begin(); itA != aprioriProbMap.end(); itA++)
	{
		ETERM *element = makeResultElemApriori(itA->first, itA->second);
		resultApriori = erl_cons(element, resultApriori);
	}
	
	ETERM* resultArr[3];
	resultArr[0] = resultNominal;
	resultArr[1] = resultContinuous;
	resultArr[2] = resultApriori;
	
	//fprintf(logFile, "\n[%0.3f]  resultNominal = ", getCPUTime());
	//erl_print_term(logFile, resultNominal);
	//fprintf(logFile, "\n[%0.3f]  resultContinuous = ", getCPUTime());
	//erl_print_term(logFile, resultContinuous);
	//fprintf(logFile, "\n[%0.3f]  resultApriori = ", getCPUTime());
	//erl_print_term(logFile, resultApriori);
	//fprintf(logFile, "\n");
	//fflush(logFile);
	
	return erl_mk_tuple(resultArr, 3);
}


ETERM *NaiveWorker::computeConditional(ETERM *attributes, ETERM *tes)
{
	if (logFile)
	{
		fprintf(logFile, "[%0.3f]  attributes = ", getCPUTime());
		erl_print_term(logFile, attributes);
		fprintf(logFile, "\n[%0.3f]  trainingExamples[0] = ", getCPUTime());
		erl_print_term(logFile, erl_hd(tes));
		fprintf(logFile, "\n");
		fflush(logFile);
	}
	
	int numAttr = erl_length(attributes);
	int *attr = attributesToArray(attributes);
	int numTES = erl_length(tes);
	
	for (int i = 0; i < numTES; i++)
	{
		ETERM *example = erl_hd(tes);
		if (ERL_IS_INTEGER(erl_element(1, example))) // numbered
			example = erl_element(2, example);
			
		ETERM *values = erl_element(1, example);
		ETERM *category = erl_element(2, example);
		char *categoryStr = getAtomStr(category);
		int categoryHash = getHash(categoryStr);
		
		aprioriProbMap[categoryHash]++;
		
		for (int i = 0; i < numAttr; i++)
		{
			if (attr[i] == ATTR_NOMINAL)
			{
				char *valueStr = getAtomStr( erl_element(i+1, values) );
				int valueHash = getHash(valueStr);
				//fprintf(logFile, "%s\t%d\t%s\n", categoryStr, i, valueStr);
				probNominalMap[MapKeyNominal(categoryHash, i, valueHash)]++;
			}
			else if (attr[i] == ATTR_CONTINUOUS)
			{
				double value = ERL_FLOAT_VALUE(erl_element(i+1, values));
				map<MapKeyContinuous, DoublePair>::iterator it = probContinuousMap.find(MapKeyContinuous(categoryHash, i));
				if (it == probContinuousMap.end())
				{
					probContinuousMap[MapKeyContinuous(categoryHash, i)] = DoublePair(value, value * value);
				}
				else
				{
					it->second.first += value;
					it->second.second += value * value;
				}
			}
		}
		
		tes = erl_tl(tes);
	}
	
	delete [] attr;
	
	return makeResult();
}


int main()
{
	ETERM* atts;
	ETERM* tes;
	ETERM* result;
	byte* buf;

	NaiveWorker *naiveWorker = new NaiveWorker("port/naive_logs");

	erl_init(NULL, 0);
  
  	// read attributes
	if (ErlComm::read_cmd(&buf) > 0)
	{
		naiveWorker->log("Attributes read to buf");
		atts = erl_decode(buf);
		naiveWorker->log("Attributes decoded");
		free(buf);
	}
	else
	{
		naiveWorker->log("Could not read attributes");
		return -1;
	}

	// read tes
	if (ErlComm::read_cmd(&buf) > 0)
	{
		naiveWorker->log("Tes read to buf");
		tes = erl_decode(buf);
		naiveWorker->log("Tes decoded");
		free(buf);
		
		result = naiveWorker->computeConditional(atts, tes);
		naiveWorker->log("Conditional probabilities computed");
		buf = (byte*)malloc(erl_term_len(result));
		erl_encode(result, buf);
		naiveWorker->log("Result encoded");
		ErlComm::write_cmd(buf, erl_term_len(result));
		naiveWorker->log("Command written");
		free(buf);
	}
	else
	{
		naiveWorker->log("Could not read tes");
		return -1;
	}

	naiveWorker->log("Cleaning up");
	erl_free_compound(tes);
	erl_free_compound(atts);
	erl_free_compound(result);
	delete naiveWorker;

	return 0;
}

