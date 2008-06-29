CFLAGS = -I/usr/local/include -D_GNU_SOURCE=1 -Dmain=SDL_main -DHAVE_OPENGL
GL_LIBS = -lopengl32 -lglu32 #-lglut -lglaux -lglu32 -lglut32 
SDL_LIBS = -L/usr/local/lib -lmingw32 -lSDLmain -lSDL -mwindows -static -lwinmm 
CC = g++
#CC = gcc
TARGET = a.exe
SRCS = $(TARGET:.exe=.c)
OBJS = $(SRCS:.c=.o)
all : $(TARGET)

$(TARGET) : $(OBJS)
	$(CC) -o $@ $(LDFLAGS) $(OBJS) $(GL_LIBS) $(SDL_LIBS) 


%.o : %.c 
	$(CC) -O2 -o $@ -c $< $(CFLAGS)

run : $(TARGET)
	start $<

# for headers
a.o : utils.hpp
