package gnu.x11.extension.glx;

import gnu.x11.Data;
import gnu.util.Misc;
import gnu.util.Vector3f;


/**
 * OpenGL toolkit library. The specification can be found <a href=
 * "http://escher.sourceforge.net/etc/specification/glut-3.ps.gz"
 * >here</a>.
 *
 * <p>Modified from <code>glut_*.c</code> in <a href=
 * "http://reality.sgi.com/opengl/#glut>
 * glut</a> by Mark J. Kilgard.
 */
public class GLUT {             // TODO
  public GL gl;
  public GLU glu;


  public GLUT (GLU glu) {
    this.glu = glu;
    gl = glu.gl;
  }


  private static final float [] [] BOX_NORMALS = {
    {-1.0f, 0.0f, 0.0f},
    {0.0f, 1.0f, 0.0f},
    {1.0f, 0.0f, 0.0f},
    {0.0f, -1.0f, 0.0f},
    {0.0f, 0.0f, 1.0f},
    {0.0f, 0.0f, -1.0f}
  };
    

  private static final int [] [] BOX_FACES = {
    {0, 1, 2, 3},
    {3, 2, 6, 7},
    {7, 6, 5, 4},
    {4, 5, 1, 0},
    {5, 6, 2, 1},
    {7, 4, 0, 3}
  };


  private void box (float size, int type) {
    float [] [] v = new float [8] [3];
    v[0][0] = v[1][0] = v[2][0] = v[3][0] = -size / 2;
    v[4][0] = v[5][0] = v[6][0] = v[7][0] = size / 2;
    v[0][1] = v[1][1] = v[4][1] = v[5][1] = -size / 2;
    v[2][1] = v[3][1] = v[6][1] = v[7][1] = size / 2;
    v[0][2] = v[3][2] = v[4][2] = v[7][2] = -size / 2;
    v[1][2] = v[2][2] = v[5][2] = v[6][2] = size / 2;


    for (int i=5; i>=0; i--) {
      gl.begin (type);
      gl.normal3fv (BOX_NORMALS [i]);
      gl.vertex3fv (v [BOX_FACES [i][0]]);
      gl.vertex3fv (v [BOX_FACES [i][1]]);
      gl.vertex3fv (v [BOX_FACES [i][2]]);
      gl.vertex3fv (v [BOX_FACES [i][3]]);
      gl.end();
    }    
  }


  // centered at origin with radius = 1.0f
  private static final float [] [] OCTAHEDRON_NORMALS = {
    {1.0f, 0.0f, 0.0f},
    {-1.0f, 0.0f, 0.0f},
    {0.0f, 1.0f, 0.0f},
    {0.0f, -1.0f, 0.0f},
    {0.0f, 0.0f, 1.0f},
    {0.0f, 0.0f, -1.0f}
  };


  private static final int [] [] OCTAHEDRON_FACES = {
    {0, 4, 2},
    {1, 2, 4},
    {0, 3, 4},
    {1, 4, 3},
    {0, 2, 5},
    {1, 5, 2},
    {0, 5, 3},
    {1, 3, 5}
  };


  private void octahedron (int type) {
    for (int i=7; i>=0; i--) {
      int [] face = OCTAHEDRON_FACES [i];
      float [] x0 = OCTAHEDRON_NORMALS [face [0]]; 
      float [] x1 = OCTAHEDRON_NORMALS [face [1]]; 
      float [] x2 = OCTAHEDRON_NORMALS [face [2]]; 
      triangle (x0, x1, x2, type);
    }
  }


  /**
   * @see <a href="glutSolidCone.html">glutSolidCone</a>
   */
  public void solid_cone (double base_radius, double height, 
    int slices, int stacks) {

    Quadric quadric = new Quadric (gl);
    quadric.draw_style = Quadric.FILL;
    quadric.normals = GLU.SMOOTH;
    quadric.cylinder (base_radius, 0.0f, height, slices, stacks);
  }


  /**
   * @see <a href="glutSolidCube.html">glutSolidCube</a>
   */
  public void solid_cube (float size) {
    box (size, GL.QUADS);
  }


  /**
   * @see <a href="glutSolidSphere.html">glutSolidSphere</a>
   */
  public void solid_sphere (double radius, int slices, int stacks) {
    Quadric quadric = new Quadric (gl);
    quadric.draw_style = Quadric.FILL;
    quadric.normals = GLU.SMOOTH;
    quadric.sphere (radius, slices, stacks);
  }


  /**
   * @see <a href="glutSolidOctahedron.html">glutSolidOctahedron</a>
   */
  public void solid_octahedron () {
    octahedron (GL.TRIANGLES);
  }


  /**
   * @see <a href="glutSolidTeapot.html">glutSolidTeapot</a>
   */
  public void solid_teapot (double scale) {
    teapot (14, scale, GL.FILL);
  }


  /**
   * @see <a href="glutSolidTorus.html">glutSolidTorus</a>
   */
  public void solid_torus (double inner_radius, double outer_radius,
    int sides, int rings) {

    float ring_delta = (float) (2.0 * Math.PI / rings);
    float side_delta = (float) (2.0 * Math.PI / sides);

    float theta = 0.0f;
    float cos_theta = 1.0f;
    float sin_theta = 0.0f;

    for (int i=rings-1; i>=0; i--) {
      float theta1 = theta + ring_delta;
      float cos_theta1 = (float) Math.cos (theta1);
      float sin_theta1 = (float) Math.sin (theta1);

      gl.begin (GL.QUAD_STRIP);
      float phi = 0.0f;

      for (int j=sides; j>=0; j--) {
        phi += side_delta;
        float cos_phi = (float) Math.cos (phi);
        float sin_phi = (float) Math.sin (phi);
        float L = (float) (outer_radius + inner_radius * cos_phi);
        float z = (float) (inner_radius * sin_phi);

        gl.normal3f (cos_theta1*cos_phi, -sin_theta1*cos_phi, sin_phi);
        gl.vertex3f (cos_theta1*L, -sin_theta1*L, z);
        gl.normal3f (cos_theta*cos_phi, -sin_theta*cos_phi, sin_phi);
        gl.vertex3f (cos_theta*L, -sin_theta*L, z);
      }
        
      gl.end ();
      theta = theta1;
      cos_theta = cos_theta1;
      sin_theta = sin_theta1;
    }
  }


  private static final int [] [] TEAPOT_PATCH = {
    /* rim */
    {102, 103, 104, 105, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15},
    /* body */
    {12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27},
    {24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40},

    /* lid */
    {96, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101, 0, 1, 2, 3,},
    {0, 1, 2, 3, 106, 107, 108, 109, 110, 
     111, 112, 113, 114, 115, 116, 117},

    /* bottom */
    {118, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 
     40, 39, 38, 37},

    /* handle */
    {41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56},
    {53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67},

    /* spout */
    {68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83},
    {80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95}
  };


  private static final float [] [] TEAPOT_CONTROL = {
    {0.2f, 0.0f, 2.7f}, {0.2f, -0.112f, 2.7f},
    {0.112f, -0.2f, 2.7f}, {0.0f, -0.2f, 2.7f},
    {1.3375f, 0.0f, 2.53125f}, {1.3375f, -0.749f, 2.53125f},
    {0.749f, -1.3375f, 2.53125f}, {0.0f, -1.3375f, 2.53125f},
    {1.4375f, 0.0f, 2.53125f}, {1.4375f, -0.805f, 2.53125f},
    {0.805f, -1.4375f, 2.53125f}, {0.0f, -1.4375f, 2.53125f},
    {1.5f, 0.0f, 2.4f}, {1.5f, -0.84f, 2.4f},
    {0.84f, -1.5f, 2.4f}, {0.0f, -1.5f, 2.4f},
    {1.75f, 0.0f, 1.875f}, {1.75f, -0.98f, 1.875f},
    {0.98f, -1.75f, 1.875f}, {0.0f, -1.75f, 1.875f},
    {2f, 0.0f, 1.35f}, {2f, -1.12f, 1.35f},
    {1.12f, -2f, 1.35f}, {0.0f, -2f, 1.35f},
    {2f, 0.0f, 0.9f}, {2f, -1.12f, 0.9f},
    {1.12f, -2f, 0.9f}, {0.0f, -2f, 0.9f},
    {-2f, 0.0f, 0.9f}, {2f, 0.0f, 0.45f},
    {2f, -1.12f, 0.45f}, {1.12f, -2f, 0.45f},
    {0.0f, -2f, 0.45f}, {1.5f, 0.0f, 0.225f},
    {1.5f, -0.84f, 0.225f}, {0.84f, -1.5f, 0.225f},
    {0.0f, -1.5f, 0.225f}, {1.5f, 0.0f, 0.15f},
    {1.5f, -0.84f, 0.15f}, {0.84f, -1.5f, 0.15f},
    {0.0f, -1.5f, 0.15f}, {-1.6f, 0.0f, 2.025f},
    {-1.6f, -0.3f, 2.025f}, {-1.5f, -0.3f, 2.25f},
    {-1.5f, 0.0f, 2.25f}, {-2.3f, 0.0f, 2.025f},
    {-2.3f, -0.3f, 2.025f}, {-2.5f, -0.3f, 2.25f},
    {-2.5f, 0.0f, 2.25f}, {-2.7f, 0.0f, 2.025f},
    {-2.7f, -0.3f, 2.025f}, {-3f, -0.3f, 2.25f},
    {-3f, 0.0f, 2.25f}, {-2.7f, 0.0f, 1.8f},
    {-2.7f, -0.3f, 1.8f}, {-3f, -0.3f, 1.8f},
    {-3f, 0.0f, 1.8f}, {-2.7f, 0.0f, 1.575f},
    {-2.7f, -0.3f, 1.575f}, {-3f, -0.3f, 1.35f},
    {-3f, 0.0f, 1.35f}, {-2.5f, 0.0f, 1.125f},
    {-2.5f, -0.3f, 1.125f}, {-2.65f, -0.3f, 0.9375f},
    {-2.65f, 0.0f, 0.9375f}, {-2f, -0.3f, 0.9f},
    {-1.9f, -0.3f, 0.6f}, {-1.9f, 0.0f, 0.6f},
    {1.7f, 0.0f, 1.425f}, {1.7f, -0.66f, 1.425f},
    {1.7f, -0.66f, 0.6f}, {1.7f, 0.0f, 0.6f},
    {2.6f, 0.0f, 1.425f}, {2.6f, -0.66f, 1.425f},
    {3.1f, -0.66f, 0.825f}, {3.1f, 0.0f, 0.825f},
    {2.3f, 0.0f, 2.1f}, {2.3f, -0.25f, 2.1f},
    {2.4f, -0.25f, 2.025f}, {2.4f, 0.0f, 2.025f},
    {2.7f, 0.0f, 2.4f}, {2.7f, -0.25f, 2.4f},
    {3.3f, -0.25f, 2.4f}, {3.3f, 0.0f, 2.4f},
    {2.8f, 0.0f, 2.475f}, {2.8f, -0.25f, 2.475f},
    {3.525f, -0.25f, 2.49375f}, {3.525f, 0.0f, 2.49375f},
    {2.9f, 0.0f, 2.475f}, {2.9f, -0.15f, 2.475f},
    {3.45f, -0.15f, 2.5125f}, {3.45f, 0.0f, 2.5125f},
    {2.8f, 0.0f, 2.4f}, {2.8f, -0.15f, 2.4f},
    {3.2f, -0.15f, 2.4f}, {3.2f, 0.0f, 2.4f},
    {0.0f, 0.0f, 3.15f}, {0.8f, 0.0f, 3.15f},
    {0.8f, -0.45f, 3.15f}, {0.45f, -0.8f, 3.15f},
    {0.0f, -0.8f, 3.15f}, {0.0f, 0.0f, 2.85f},
    {1.4f, 0.0f, 2.4f}, {1.4f, -0.784f, 2.4f},
    {0.784f, -1.4f, 2.4f}, {0.0f, -1.4f, 2.4f},
    {0.4f, 0.0f, 2.55f}, {0.4f, -0.224f, 2.55f},
    {0.224f, -0.4f, 2.55f}, {0.0f, -0.4f, 2.55f},
    {1.3f, 0.0f, 2.55f}, {1.3f, -0.728f, 2.55f},
    {0.728f, -1.3f, 2.55f}, {0.0f, -1.3f, 2.55f},
    {1.3f, 0.0f, 2.4f}, {1.3f, -0.728f, 2.4f},
    {0.728f, -1.3f, 2.4f}, {0.0f, -1.3f, 2.4f},
    {0.0f, 0.0f, 0.0f}, {1.425f, -0.798f, 0.0f},
    {1.5f, 0.0f, 0.075f}, {1.425f, 0.0f, 0.0f},
    {0.798f, -1.425f, 0.0f}, {0.0f, -1.5f, 0.075f},
    {0.0f, -1.425f, 0.0f}, {1.5f, -0.84f, 0.075f},
    {0.84f, -1.5f, 0.075f}
  };


  private static final float [] [] [] TEAPOT_TEXTURE = {
    {{0.0f, 0.0f},
     {1.0f, 0.0f}},
    {{0.0f, 1.0f},
     {1.0f, 1.0f}}
  };


  private void teapot (int grid, double scale, int type) {
    gl.push_attrib (GL.ENABLE_BIT | GL.EVAL_BIT);
    gl.push_matrix ();

    gl.enable (GL.AUTO_NORMAL);
    gl.enable (GL.NORMALIZE);
    gl.enable (GL.MAP2_VERTEX_3);
    gl.enable (GL.MAP2_TEXTURE_COORD_2);
    gl.rotatef (270.0f, 1.0f, 0.0f, 0.0f);    
    float scale0 = (float) (0.5 * scale);
    gl.scalef (scale0, scale0, scale0);
    gl.translatef (0.0f, 0.0f, -1.5f);    
    teapot_patch (grid, type);
    
    gl.pop_matrix ();
    gl.pop_attrib ();
  }

  
  private void teapot_patch (int grid, int type) {
    float [] [] [] p = new float [4] [4] [3];
    float [] [] [] q = new float [4] [4] [3];
    float [] [] [] r = new float [4] [4] [3];
    float [] [] [] s = new float [4] [4] [3];

    for (int i=0; i<10; i++) {
      for (int j=0; j<4; j++)
        for (int k=0; k<4; k++)
          for (int l=0; l<3; l++) {
            int index0 = TEAPOT_PATCH [i] [j * 4 + k];
            int index1 = TEAPOT_PATCH [i] [j * 4 +  (3 - k)];

            p [j] [k] [l] = TEAPOT_CONTROL [index0] [l];
            q [j] [k] [l] = TEAPOT_CONTROL [index1] [l];
            if  (l == 1) q [j] [k] [l] *= -1.0f;

            if  (i < 6) {
              r [j] [k] [l] = TEAPOT_CONTROL [index1] [l];
              if  (l == 0) r [j] [k] [l] *= -1.0;

              s [j] [k] [l] = TEAPOT_CONTROL [index0] [l];
              if  (l == 0 || l == 1) s [j] [k] [l] *= -1.0;
            }
          }

      float [] p0 = Misc.linearize (p);
      float [] q0 = Misc.linearize (q);
      float [] r0 = Misc.linearize (r);
      float [] s0 = Misc.linearize (s);
      float [] t0 = Misc.linearize (TEAPOT_TEXTURE);

      gl.map2f (GL.MAP2_TEXTURE_COORD_2, 0, 1, 2, 2, 0, 1, 4, 2, t0);        
      gl.map2f (GL.MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, p0);
      gl.map_grid2f (grid, 0.0f, 1.0f, grid, 0.0f, 1.0f);
      gl.eval_mesh2 (type, 0, grid, 0, grid);
      gl.map2f (GL.MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, q0);
      gl.eval_mesh2 (type, 0, grid, 0, grid);

      if  (i<6) {
        gl.map2f (GL.MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, r0);
        gl.eval_mesh2 (type, 0, grid, 0, grid);
        gl.map2f (GL.MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, s0);
        gl.eval_mesh2 (type, 0, grid, 0, grid);
      }
    }
  }


  private void triangle (float [] x0, float [] x1, float [] x2, int type) {
    // TODO subdivide
    Vector3f A = new Vector3f (x0).normalize ();
    Vector3f B = new Vector3f (x1).normalize ();
    Vector3f C = new Vector3f (x2).normalize ();

    Vector3f AB = new Vector3f ().minus (A, B);
    Vector3f AC = new Vector3f ().minus (A, C);
    Vector3f n = new Vector3f ().cross (AC, AB).normalize ();

    gl.begin (type);
    gl.normal3fv (n.v);
    gl.vertex3fv (x0);
    gl.vertex3fv (x1);
    gl.vertex3fv (x2);
    gl.end ();
  }

    
  /**
   * @see <a href="glutWireCone.html">glutWireCone</a>
   */
  public void wire_cone (double base_radius, double height, 
    int slices, int stacks) {

    Quadric quadric = new Quadric (gl);
    quadric.draw_style = Quadric.LINE;
    quadric.normals = GLU.SMOOTH;
    quadric.cylinder (base_radius, 0.0f, height, slices, stacks);
  }


  /**
   * @see <a href="glutWireCube.html">glutWireCube</a>
   */
  public void wire_cube (float size) {
    box (size, GL.LINE_LOOP);
  }


  /**
   * @see <a href="glutWireSphere.html">glutWireSphere</a>
   */
  public void wire_sphere (double radius, int slices, int stacks) {
    Quadric quadric = new Quadric (gl);
    quadric.draw_style = Quadric.LINE;
    quadric.normals = GLU.SMOOTH;
    quadric.sphere (radius, slices, stacks);
  }


  /**
   * @see <a href="glutWireOctahedron.html">glutWireOctahedron</a>
   */
  public void wire_octahedron () {
    octahedron (GL.LINE_LOOP);
  }


  /**
   * @see <a href="glutSolidTeapot.html">glutSolidTeapot</a>
   */
  public void wire_teapot (double scale) {
    teapot (10, scale, GL.FILL);
  }


  /**
   * @see <a href="glutWireTorus.html">glutWireTorus</a>
   */
  public void wire_torus (double inner_radius, double outer_radius,
    int sides, int rings) {

    gl.push_attrib (GL.POLYGON_BIT);
    gl.polygon_mode (GL.FRONT_AND_BACK, GL.LINE);
    solid_torus (inner_radius, outer_radius, sides, rings);
    gl.pop_attrib ();    
  }
}
