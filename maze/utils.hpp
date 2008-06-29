#include <GL/gl.h>
#include <GL/glu.h>

// from freeglut
void glutWireCube( GLdouble dSize )
{
    double size = dSize * 0.5;

#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );

    /* PWO: I dared to convert the code to use macros... */
    glBegin( GL_LINE_LOOP ); 
    N( 1.0, 0.0, 0.0); V(+,-,+); V(+,-,-); V(+,+,-); V(+,+,+); 
    glEnd();
    glBegin( GL_LINE_LOOP ); 
    N( 0.0, 1.0, 0.0); V(+,+,+); V(+,+,-); V(-,+,-); V(-,+,+); glEnd();
    glBegin( GL_LINE_LOOP ); 
    N( 0.0, 0.0, 1.0); V(+,+,+); V(-,+,+); V(-,-,+); V(+,-,+); glEnd();
    glBegin( GL_LINE_LOOP );
    N(-1.0, 0.0, 0.0); V(-,-,+); V(-,+,+); V(-,+,-); V(-,-,-); glEnd();
    glBegin( GL_LINE_LOOP );
    N( 0.0,-1.0, 0.0); V(-,-,+); V(-,-,-); V(+,-,-); V(+,-,+); glEnd();
    glBegin( GL_LINE_LOOP );
    N( 0.0, 0.0,-1.0); V(-,-,-); V(-,+,-); V(+,+,-); V(+,-,-); glEnd();

#   undef V
#   undef N
}

/*
 * Draws a solid cube. Code contributed by Andreas Umbach <marvin@dataway.ch>
 */
void glutSolidCube( GLdouble dSize )
{
    double size = dSize * 0.5;


#   define V(a,b,c) glVertex3d( a size, b size, c size );
#   define N(a,b,c) glNormal3d( a, b, c );

    /* PWO: Again, I dared to convert the code to use macros... */
    glBegin( GL_QUADS );
    N( 1.0, 0.0, 0.0); V(+,-,+); V(+,-,-); V(+,+,-); V(+,+,+);
        N( 0.0, 1.0, 0.0); V(+,+,+); V(+,+,-); V(-,+,-); V(-,+,+);
        N( 0.0, 0.0, 1.0); V(+,+,+); V(-,+,+); V(-,-,+); V(+,-,+);
        N(-1.0, 0.0, 0.0); V(-,-,+); V(-,+,+); V(-,+,-); V(-,-,-);
        N( 0.0,-1.0, 0.0); V(-,-,+); V(-,-,-); V(+,-,-); V(+,-,+);
        N( 0.0, 0.0,-1.0); V(-,-,-); V(-,+,-); V(+,+,-); V(+,-,-);
    glEnd();

#   undef V
#   undef N
}

// from freeglut end 

template <class T>
struct Pointt {
  T x;
  T y;
  T z;
  Pointt(){};
  Pointt(T x,T y,T z):x(x),y(y),z(z) {};

  Pointt<T> operator+(Pointt<T> const & o) {
    Pointt<T> result;
    result.x = x + o.x;
    result.y = y + o.y;
    result.z = z + o.z;
    return result;
  }

  Pointt<T> operator-(Pointt<T> const & o) {
    Pointt<T> result;
    result.x = x - o.x;
    result.y = y - o.y;
    result.z = z - o.z;
    return result;
  }

  Pointt<T> operator*(T const & o) {
    Pointt<T> result;
    result.x = this->x * o;
    result.y = this->y * o;
    result.z = this->z * o;
    return result;
  }

  Pointt<T> operator/(T const & o) {
    Pointt<T> result;
    result.x = this->x / o;
    result.y = this->y / o;
    result.z = this->z / o;
    return result;
  }
  
};
