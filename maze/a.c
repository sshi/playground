#include <iostream>
#include <math.h>
#include <time.h>
#include <stdlib.h>

#include <SDL/SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>

#include <boost/math/quaternion.hpp>
using namespace std;
using namespace boost::math;

#include "utils.hpp"

#define TEX_HEIGHT 16 
#define TEX_WIDTH 16 
#define SCREEN_BPP    32

// for texture

static GLubyte image[TEX_HEIGHT][TEX_WIDTH][4];

void initTexture(void) {
  int i, j, c;
  for (i=0;i<TEX_HEIGHT;i++) { 
    for (j=0;j<TEX_WIDTH;j++) { 
      c = ( ((i&0x01)==0)^((j&0x01)==0) ); 
      image[i][j][0]= image[i][j][1]= image[i][j][2]=c*255;
      image[i][j][3]=255; 
    } 
  } 
} 


// -----------------------------------------------------------------
// GLOVAL VARIABLES
// -----------------------------------------------------------------

int width = 600,height=400,s_height=10;
//int t = 0;

// -----------------------------------------------------------------
// classes
// -----------------------------------------------------------------

typedef float ElemType;
typedef Pointt<ElemType> Point;
typedef quaternion<ElemType> Qn;
typedef Point Vector3;

// Utils

template <class X>
X normalize(X n) {
  if (-0.5 < n && n < 0.5) return 0;
  if (n < -0.5) return -1;
  if (n > 0.5) return  1;
}

template <class T>
Pointt<T> normalize(Pointt<T> src){
  Pointt<T> result;
  result.x = normalize(src.x);
  result.y = normalize(src.y);
  result.z = normalize(src.z);
  return result;
}

Vector3 turnQ(Vector3 src,Vector3 axis,int degree){
  double half_rad = M_PI * degree / 360;
  Qn q(cos(half_rad),
       axis.x * sin(half_rad),
       axis.y * sin(half_rad),
       axis.z * sin(half_rad));
  
  Qn r(cos(half_rad),
       -axis.x * sin(half_rad),
       -axis.y * sin(half_rad),
       -axis.z * sin(half_rad));
  
  Qn p(0,src.x,src.y,src.z);
  Qn rpq = conj(q) * p * q;
  return Vector3(rpq.R_component_2(),
		  rpq.R_component_3(),
		  rpq.R_component_4());
}

Vector3 normalvector(Vector3 a,Vector3 b){
  return Vector3(a.y * b.z - a.z * b.y,
		  a.z * b.x - a.x * b.z,
		  a.x * b.y - a.y * b.x);
}

void pv(Vector3 o){
  printf("%f,%f,%f",o.x,o.y,o.z);
}


class View {
  // position & vector
  
  Vector3 ahead;
  Vector3 upper;

  // turn & move
  bool moving;
  bool turning;

  Vector3 axis;
  short degree;
  Vector3 pos_d;
  Vector3 pos_orig;
  Vector3 ahead_orig;

  short resttime;
  short turntime;

  void beginMove(int d){
    pos_orig = pos;
    moving = true;
    resttime = turntime / 2;
    ahead_orig = ahead * d;
    
    pos_d = ahead_orig / resttime;
  };

  void endMove(){
    moving = false;
    pos = pos_orig + ahead_orig;
  }

  void beginTurn(Vector3 axis,int rotate_dir){
    turning = true;
    resttime = turntime;
    degree = rotate_dir * 90 / turntime;
    this->axis = axis;
  };

  void endTurn(){
    turning = false;
    ahead = normalize(ahead);
    upper = normalize(upper);
  }
  
 public:
  Point pos;
  View(){
    ahead = Vector3(0,0,-1);
    upper = Vector3(0,1,0);
    pos = Point(0,0,-10);
    moving = false;
    turning = false;
    resttime = turntime = 30;
  };

  Vector3 get_light(){
    //return pos + (ahead * 3) + upper;
    //return pos + upper * 10;
    //return pos;
    return Point(0,20,0);
  };

  
  bool isMoving(){
    return moving;
  }
  
  bool isTurning(){
    return turning;
  }
 
  void process(){
    if (this->isTurning()){
      resttime -= 1;
      if (resttime == 0){
	endTurn();
      } else {
	ahead = turnQ(ahead,axis,degree);
	upper = turnQ(upper,axis,degree);
      }
    }
    if (this->isMoving()) {
      resttime -= 1;
      if (resttime == 0){
	endMove();
      } else {
	pos = pos + pos_d;
      }
    }
    return;
  };

  void lookAt(){
    Vector3 viewp = pos + ahead;

    gluLookAt(pos.x,pos.y,pos.z,
	      viewp.x,viewp.y,viewp.z,
	      upper.x,
	      upper.y,
	      upper.z);
  };


  // move & turn interface

  void beginGoAhead(){
    beginMove(1);
  }

  void beginBack(){
    beginMove(-1);
  }

  void beginTurnRight(){
    beginTurn(upper,1);
  }
  void beginTurnLeft(){
    beginTurn(upper,-1);
  }
  void beginTurnDown(){
    Vector3 nv = normalvector(ahead,upper);
    beginTurn(nv,1);
  }
  void beginTurnUp(){
    Vector3 nv = normalvector(ahead,upper);
    beginTurn(nv,-1);
  }
  void beginRollRight(){
    beginTurn(ahead,1);
  }
  void beginRollLeft(){
    beginTurn(ahead,-1);
  }
};


void drawSpinCube(int clock){
  glPushMatrix(); 
  {
    glTranslated(0,-1.0,0);
    glRotated(clock*2,0,0,1);
    glTranslated(2.0,0.0,-10.0);
    glRotated(-clock*5,0,1,0);
    glPushMatrix();
    glLoadIdentity();
    glPopMatrix();
    glutSolidCube(1);
  }
  glPopMatrix();
  return;
}


void drawCube(double x,double y,double z,double size){
  glPushMatrix();
  {
    glTranslated(x,y,z);
    glutSolidCube(size);
  }
  glPopMatrix();
  return;
}

void drawTile(){
  double size = 0.5;
#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );
#   define T(a,b)   glTexCoord2f(a,b);
  //#define T(a,b)  ;

  glEnable(GL_TEXTURE_2D); 
  glBegin( GL_QUADS ); {
    N( 1.0, 0.0, 0.0); 
    T(0.0,1.0);V(+,-,+); 
    T(0.0,0.0);V(+,-,-); 
    T(1.0,0.0);V(+,+,-); 
    T(1.0,1.0);V(+,+,+);
    
    N( 0.0, 1.0, 0.0); 
    T(1.0,1.0);V(+,+,+);
    T(1.0,0.0);V(+,+,-);
    T(0.0,0.0);V(-,+,-);
    T(0.0,1.0);V(-,+,+);
    
    N( 0.0, 0.0, 1.0); 
    T(1.0,1.0);V(+,+,+); 
    T(0.0,1.0);V(-,+,+); 
    T(0.0,0.0);V(-,-,+); 
    T(1.0,0.0);V(+,-,+);
    
    N(-1.0, 0.0, 0.0); 
    T(0.0,1.0);V(-,-,+); 
    T(1.0,1.0);V(-,+,+); 
    T(1.0,0.0);V(-,+,-); 
    T(0.0,0.0);V(-,-,-);

    N( 0.0,-1.0, 0.0);
    T(0.0,1.0);V(-,-,+);
    T(0.0,0.0);V(-,-,-);
    T(1.0,0.0);V(+,-,-); 
    T(1.0,1.0);V(+,-,+);
    
    N( 0.0, 0.0,-1.0);
    T(0.0,0.0);V(-,-,-); 
    T(0.0,1.0);V(-,+,-); 
    T(1.0,1.0);V(+,+,-); 
    T(1.0,0.0);V(+,-,-);
    
  }
  glEnd(); 
  glDisable(GL_TEXTURE_2D); 
#   undef V
#   undef N
# undef T
  return;
}

void drawCubeMatrix(int t){
  glPushMatrix();
  glTranslated(0,0,-5);
  glRotated(t,1,0,0);
  glRotated(t*2,0,1,0);
  for (int i = 0;i < 3;i++){
    for (int j = 0;j < 3;j++){
      for (int k = 0;k < 3;k++){
	drawCube((double)i-1,(double)j-1,(double)k-1,0.5);
      }
    }
  }
  glPopMatrix();
  return;
}

void drawBoard(Point pos,double x,double y,double z){
  glPushMatrix();
  glTranslated(pos.x,pos.y,pos.z);
  glScaled(x,y,z);
  drawTile();
  glPopMatrix();
  return;
}

class SpinCube{
  int clock;
 public:
  SpinCube(){
    clock = 0;
  }
  void process(void){
    clock +=1;
  }
  void draw(){
    drawSpinCube(clock);
  }
};

class CubeMatrix{
  int clock;
 public:
  CubeMatrix(){
    clock = 0;
  }
  void process(void){
    clock +=1;
  }
  void draw(){
    drawCubeMatrix(clock);
  }
};

class Maze {
  const static int size = 5;
  Point field[size][size][size];
 public:
  Maze(){
    
  }
  //process(){}
  void init(){
    for (int i = 0;i < size;i++){
      for (int j = 0;j < size;j++){
	for (int k = 0;k < size;k++){
	  int x,y,z;
	  x = (rand() % 2 == 0)?0:1;
	  y = (rand() % 2 == 0)?0:1;
	  z = (rand() % 2 == 0)?0:1;
	  field[i][j][k] = Point(x,y,z);
	}
      }
    }
  }
  void draw(){
    for (int i = 0;i < size;i++){
      for (int j = 0;j < size;j++){
	for (int k = 0;k < size;k++){
	  Point t = field[i][j][k];
	  if (t.x == 1) drawBoard(Point(i-0.5,j,k),0.1,1,1);
	  if (t.y == 1) drawBoard(Point(i,j-0.5,k),1,0.1,1);
	  if (t.z == 1) drawBoard(Point(i,j,k-0.5),1,1,0.1);
	}
      }
    }
    return;
  }
};


// -----------------------------------------------------------------
// GLOBAL STATE
// -----------------------------------------------------------------

View v;
SpinCube sc;
CubeMatrix cm;
Maze mz;

// -----------------------------------------------------------------

void set_light(void){
  Vector3 light = v.get_light();
  float light0[] = {light.x,light.y,light.z,1.0};
  glLightfv(GL_LIGHT0,GL_POSITION,light0);
  glEnable(GL_LIGHTING);  
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);
}


void projection2D(){
  glViewport(0,0,width,s_height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0,width,0,s_height);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glEnable(GL_POINT_SMOOTH);
  glDisable(GL_LIGHTING);
}

void projection3D(){
  glViewport(0,s_height,width,height);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0,(double)width/(double)height,0.1,100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glShadeModel(GL_SMOOTH);
  
  // move view
  v.process();
  v.lookAt();
  set_light();
}


// -----------------------------------------------------------------
// Drawing Objects 
// -----------------------------------------------------------------


void drawSolidPlain(double y,double size,double range){
  double l = size * 0.5;
  int count = (int)(size / range);
  glBegin( GL_QUADS );
  {
    for (double z = 0;z<count;z++){
      for (double x = 0;x<count;x++){
	glNormal3d(0,1,0);
	glVertex3d( -l+x*range,y,-l+z*range);
	glVertex3d( -l+x*range,y,-l+(z+1)*range);
	
	glVertex3d( -l+(x+1)*range,y,-l+(z+1)*range);
	glVertex3d( -l+(x+1)*range,y,-l+z*range);

      }
    }
  }
  glEnd();
}

void drawFps(void){
  int max_fps = 120;
  static int pre_time = 0;
  int current = SDL_GetTicks();
  int fps = 1000 / (current - pre_time);
  if (fps > max_fps) {
    fps = max_fps;
  }
  
  glPointSize(10.0);
  glBegin(GL_POINTS);{
    glVertex2d(width * fps / max_fps,5);
  }
  glEnd();
  pre_time = current;
}



// ---------------------------------------------------------------
// Main
// ---------------------------------------------------------------


bool process_events(void){
  SDL_Event ev;
  SDLKey *key;
  
  while(SDL_PollEvent(&ev) ) {
    switch(ev.type){
    case SDL_QUIT:// ウィンドウの×ボタンが押された時など
      return false;
    case SDL_KEYDOWN:// キーボードからの入力があった時
      key=&(ev.key.keysym.sym);
      if(*key==27){
	return false;
      }
      if(!v.isMoving() && !v.isTurning()) {
	switch(ev.key.keysym.sym){
	case SDLK_SPACE:
	  v.beginGoAhead();
	  break;
	case SDLK_LEFT:
	  v.beginTurnLeft();
	  break;
	case SDLK_RIGHT:
	  v.beginTurnRight();
	  break;
	case SDLK_UP:
	  v.beginGoAhead();
	  break;
	case SDLK_DOWN:
	  v.beginBack();
	  break;
	case SDLK_z:
	  v.beginRollRight();
	  break;
	case SDLK_c:
	  v.beginRollLeft();
	  break;
	case SDLK_x:
	  v.beginTurnDown();
	  break;
	case SDLK_s:
	  v.beginTurnUp();
	  break;
	}
      }
      break;
    }
  }
  return true;
}

void draw3D(){
  drawSolidPlain(-10,100,1);
  mz.draw();
  //sc.draw();
  //cm.draw();
}

void display(void){
  // process
  sc.process();
  cm.process();

  // draw
  glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

  projection2D();
  drawFps();

  projection3D();
  draw3D();

  SDL_GL_SwapBuffers( );
  //t += 1;
}

void main_loop(void){
  mz.init();
  while(1) {
    if (!process_events())  break;
    display();
    pv(v.pos);
    printf("\n");
  }
}

// ------------------------------------------------------------------
// Initialize
// ------------------------------------------------------------------

void gl_init(void){
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  initTexture();
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); 
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 
	       TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, image); 
}

int main(int argc,char** argv){
  srand((unsigned)time(NULL));
  // SDL_INIT
  SDL_Surface *gScreenSurface = SDL_SetVideoMode(width,
						 height+s_height,
						 SCREEN_BPP,
						 SDL_SWSURFACE | SDL_OPENGL);
  SDL_WM_SetCaption( "SDL & OpenGL Study",NULL);
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) {
    printf( "初期化に失敗しました" );
    return 0;
  }
  // SDL_INIT END
  gl_init();
  main_loop();
  SDL_Quit();
  return 0;
}
