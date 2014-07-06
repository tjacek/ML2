CXX = g++
INCLUDE_PATH = -I/usr/local/lib/erlang/lib/erl_interface-3.7.6/include
LIB_PATH = -L/usr/local/lib/erlang/lib/erl_interface-3.7.6/lib
CXXFLAGS = -Wall $(INCLUDE_PATH) $(LIB_PATH)
LDFLAGS = -O2
PROGRAM = naive_worker
SRCS  = naive_worker.cpp erl_comm.cpp mllib.cpp
OBJS  = $(SRCS:.cpp=.o)
LIBRARIES = -lerl_interface -lei -pthread

$(PROGRAM): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(PROGRAM) $(LDFLAGS) $(OBJS) $(LIBRARIES)

clean:
	rm -f $(PROGRAM) *.o

