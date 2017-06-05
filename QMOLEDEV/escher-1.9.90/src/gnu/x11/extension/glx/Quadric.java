package gnu.x11.extension.glx;

/**
 * GLU quadric object. 
 *
 * <p>Modified from <code>src-glu/quadric.c</code> in <a href=
 * "http://mesa3d.org/">Mesa 3D</a> by Brian Paul.
 */
public class Quadric {
  public static final int SMOOTH = 100000;
  public static final int FLAT = 100001;
  public static final int NONE = 100002;
  public static final int POINT = 100010;
  public static final int LINE = 100011;
  public static final int FILL = 100012;
  public static final int SILHOUETTE = 100013;
  public static final int OUTSIDE = 100020;
  public static final int INSIDE = 100021;


  public GL gl;

  public int draw_style;
  public int normals;
  public int orientation;
  public boolean texture;


  // synchroized variables
  private double inner_radius, outer_radius;
  private double base_radius, top_radius;
  private double start_radian, sweep_radian;
  private double height, radius;
  private double dr, drho, dtheta, dz, nsign, nz;
  private int loops, slices, stacks;


  public Quadric (GL gl) { this.gl = gl; }
    

  /**
   * @see <a href="gluCylinder.html">gluCylinder</a>
   */
  public synchronized void cylinder (double base_radius, 
    double top_radius, double height, int slices, int stacks) {

    this.base_radius = base_radius;
    this.top_radius = top_radius;
    this.height = height;
    this.slices = slices;
    this.stacks = stacks;

    dr = (top_radius - base_radius) / stacks;
    dtheta = 2.0 * Math.PI / slices;
    dz = height / stacks;
    nsign = orientation == INSIDE ? -1.0f : 1.0f;
    nz = (base_radius - top_radius) / height;

    switch (draw_style) {
    case FILL: cylinder_fill (); break;
    case LINE: cylinder_line (); break;
    case SILHOUETTE: cylinder_silhouette (); break;
    default: throw new java.lang.Error ("");
    }
  }  
    

  private void cylinder_draw (float z, double radius) {
    for (int i=0; i<slices; i++) {
      double theta = i * dtheta;
      double x = Math.cos (theta);
      double y = Math.sin (theta);

      cylinder_normal (x, y, nz);

      float vx = (float) (x * radius);
      float vy = (float) (y * radius);
      gl.vertex3f (vx, vy, z);
    }
  }
    
    
  private void cylinder_fill () {    
    float ds = 1.0f / slices;
    float dt = 1.0f / stacks;
    float t = 0.0f;
    float z = 0.0f;
    double r = base_radius;

    for (int j=0; j<stacks; j++, r+=dr, t+=dt, z+=dz) {
      float s = 0.0f;
      gl.begin (GL.QUAD_STRIP);

      for (int i=0; i<=slices; i++, s+=ds) {
        double theta = i == slices ? 0.0f : i*dtheta;
        double x = Math.sin (theta);
        double y = Math.cos (theta);

        float vx, vy, vz;
        
        cylinder_normal (x, y, nz);
        if (texture) gl.tex_coord2f (s, t);
        vx = (float) (x * r);
        vy = (float) (y * r);
        vz = z;
        gl.vertex3f (vx, vy, vz);

        cylinder_normal (x, y, nz);
        if (texture) gl.tex_coord2f (s, t+dt);
        vx = (float) (x * (r + dr));
        vy = (float) (y * (r + dr));
        vz = (float) (z + dz);
        gl.vertex3f (vx, vy, vz);
      }

      gl.end ();
    }
  }


  private void cylinder_line () {    
    float z = 0.0f;
    double r = base_radius;

    // draw rings
    for (int j=0; j<=stacks; j++, r+=dr, z+=dz) {
      gl.begin (GL.LINE_LOOP);
      cylinder_draw (z, r);
      gl.end ();
    }


    // draw length lines
    gl.begin (GL.LINES);
    cylinder_draw (0.0f, base_radius);
    cylinder_draw ((float) height, top_radius);
    gl.end ();
  }


  private void cylinder_normal (double x, double y, double z) {
    double length = Math.sqrt (x*x + y*y + z*z);
    if (length != 0.0f) {
      x /= length;
      y /= length;
      z /= length;
    }    
    gl.normal3f ((float) x, (float) y, (float) z);
  }


  private void cylinder_silhouette () {
    // draw one ring at each end
    if (base_radius != 0.0) {
      gl.begin (GL.LINE_LOOP);
      cylinder_draw (0.0f, base_radius);
      gl.end ();

      gl.begin (GL.LINE_LOOP);
      cylinder_draw ((float) height, top_radius);
      gl.end ();
    }


    // draw length lines
    gl.begin (GL.LINES);
    cylinder_draw ((float) height, base_radius);
    cylinder_draw (0.0f, top_radius);
    gl.end ();
  }

  
  /**
   * @see <a href="gluDisk.html">gluDisk</a>
   */
  public synchronized void disk (double inner_radius, double outer_radius,
    int slices, int loops) {

    this.inner_radius = inner_radius;
    this.outer_radius = outer_radius;
    this.slices = slices;
    this.loops = loops;

    dr = (outer_radius - inner_radius) / loops;
    dtheta = 2.0 * Math.PI / slices;

    if (normals != NONE)
      if (orientation == OUTSIDE) gl.normal3f (0.0f, 0.0f, 1.0f);
      else gl.normal3f (0.0f, 0.0f, -1.0f);        

    switch (draw_style) {
    case LINE: disk_line (); break;
    default: throw new java.lang.Error ("");
    }
  }


  private void disk_line () {
    double r, theta;

    // loops
    r = inner_radius;
    for (int l=0; l<=loops; l++, r+=dr) {
      gl.begin (GL.LINE_LOOP);

      theta = 0;
      for (int s=0; s<slices; s++, theta+=dtheta) {
        float x = (float) (r * Math.sin (theta));
        float y = (float) (r * Math.cos (theta));
        gl.vertex2f (x, y);
      }

      gl.end ();
    }


    // spokes
    theta = 0;
    for (int s=0; s<slices; s++, theta+=dtheta) {
      double x = (float) Math.sin (theta);
      double y = (float) Math.cos (theta);
      gl.begin (GL.LINE_STRIP);

      r = inner_radius;            
      for (int l=0; l<=loops; l++, r+=dr) {
        gl.vertex2f ((float) (r*x), (float) (r*y));
      }

      gl.end ();
    }
  }


  /**
   * @see <a href="gluPartialDisk.html">gluPartialDisk</a>
   */
  public synchronized void partial_disk (double inner_radius, 
    double outer_radius, int slices, int loops, 
    double start, double sweep) {

    this.inner_radius = inner_radius;
    this.outer_radius = outer_radius;
    this.slices = slices;
    this.loops = loops;
    this.start_radian = Math.toRadians (start);
    this.sweep_radian = Math.toRadians (sweep);

    dr = (outer_radius - inner_radius) / loops;
    dtheta = sweep_radian / slices;

    if (normals != NONE)
      if (orientation == OUTSIDE) gl.normal3f (0.0f, 0.0f, 1.0f);
      else gl.normal3f (0.0f, 0.0f, -1.0f);        

    switch (draw_style) {
    case SILHOUETTE: disk_silhouette (); break;
    default: throw new java.lang.Error ("");
    }
  }


  private void disk_silhouette () {
    disk_silhouette_ring (outer_radius);
    disk_silhouette_ring (inner_radius);

    // spokes
    gl.begin (GL.LINES);
    double stop_radian = start_radian + sweep_radian;
    double a0 = Math.sin (start_radian);
    double b0 = Math.cos (start_radian);
    double a1 = Math.sin (stop_radian);
    double b1 = Math.cos (stop_radian);
    gl.vertex2d (inner_radius*a0, inner_radius*b0);
    gl.vertex2d (outer_radius*a0, outer_radius*b0);
    gl.vertex2d (inner_radius*a1, inner_radius*b1);
    gl.vertex2d (outer_radius*a1, outer_radius*b1);
    gl.end ();
  }


  private void disk_silhouette_ring (double radius) {
    double theta = start_radian;
    gl.begin (GL.LINE_STRIP);

    for (int s=0; s<=slices; s++, theta+=dtheta) {
      double x = radius * Math.sin (theta);
      double y = radius * Math.cos (theta);
      gl.vertex2d (x, y);
    }

    gl.end ();
  }

  
  /**
   * @see <a href="gluSphere.html">gluSphere</a>
   */
  public synchronized void sphere (double radius, int slices, int stacks) {
    this.radius = radius;
    this.slices = slices;
    this.stacks = stacks;

    drho = Math.PI / stacks;
    dtheta = 2.0 * Math.PI / slices;
    nsign = orientation == INSIDE ? -1.0f : 1.0f;

    /* Rendered image from Mesa is different from that of SGI reference
     * implementation, because of different vertex ordering. There is
     * visible discrepency particularly for the specular reflection with
     * GL.FLAT shading model in `accanti.c' and `accpersp.c' in redbook. 
     * Brain Paul claims both are acceptable.
     */

    switch (draw_style) {
    case FILL: sphere_fill (); break;
    case LINE:                  // fall through
    case SILHOUETTE: sphere_line (); break;
    default: throw new java.lang.Error ("");
    }
  }


  private void sphere_fill () {
    if (texture) throw new java.lang.Error (""); // TODO

    if (!texture) sphere_fill_draw_positive_z ();
    sphere_fill_draw_stack ();
    if (!texture) sphere_fill_draw_negative_z ();
  }


  private void sphere_fill_draw_slice (double theta, double rho) {
    double x = -Math.sin (theta) * Math.sin (rho);
    double y = Math.cos (theta) * Math.sin (rho);
    double z = nsign * Math.cos (rho);
    sphere_normal (x, y, z);
    
    float vx = (float) (x * radius);
    float vy = (float) (y * radius);
    float vz = (float) (z * radius);
    gl.vertex3f (vx, vy, vz);
  }


  private void sphere_fill_draw_negative_z () {
    gl.begin (GL.TRIANGLE_FAN);
    gl.normal3f (0.0f, 0.0f, -1.0f); // negative
    float z = (float) (-nsign * radius); // negative
    gl.vertex3f (0.0f, 0.0f, z);
    
    float rho = (float) (Math.PI - drho);
    for (int j=slices; j>=0; j--) {
      double theta = j == slices ? 0.0f : j*dtheta;
      sphere_fill_draw_slice (theta, rho); // vs. drho
    }
    gl.end ();
  }
    

  private void sphere_fill_draw_positive_z () {   
    gl.begin (GL.TRIANGLE_FAN);
    gl.normal3f (0.0f, 0.0f, 1.0f); // positive
    float z = (float) (nsign * radius); // positive
    gl.vertex3f (0.0f, 0.0f, z);

    for (int j=0; j<=slices; j++) {
      double theta = j == slices ? 0.0f : j*dtheta;
      sphere_fill_draw_slice (theta, drho); // vs. rho
    }
    gl.end ();
  }


  private void sphere_fill_draw_stack () {
    float ds = 1.0f / slices;
    float dt = 1.0f / stacks;    
    int imin = 1;
    int imax = stacks - 1;

    float t = 1.0f;
    for (int i=imin; i<imax; i++, t-=dt) {
      double rho = i * drho;
      float s = 0.0f;
      gl.begin (GL.QUAD_STRIP);
      
      for (int j=0; j<=slices; j++, s+=ds) {
        double theta = j == slices ? 0.0f : j * dtheta;
        sphere_fill_draw_slice (theta, rho); // vs. rho+drho
        if (texture) gl.tex_coord2f (s, t);

        sphere_fill_draw_slice (theta, rho+drho); // vs. rho
        if (texture) gl.tex_coord2f (s, t-dt);
      }

      gl.end ();
    }
  }


  private void sphere_line () {
    // draw stack lines
    for (int i=1; i<stacks; i++) {
      double rho = i * drho;     // no i==0
      gl.begin (GL.LINE_LOOP);
      
      for (int j=0; j<slices; j++) {
        double theta = j * dtheta;
        sphere_line_draw (theta, rho);
      }

      gl.end ();
    }

    // draw slice lines
    for (int j=0; j<slices; j++) {
      double theta = j * dtheta;
      gl.begin (GL.LINE_STRIP);

      for (int i=0; i<=stacks; i++) {
        double rho = i * drho;     
        sphere_line_draw (theta, rho);
      }

      gl.end ();
    }
  }


  private void sphere_line_draw (double theta, double rho) {
    double x = Math.cos (theta) * Math.sin (rho);
    double y = Math.sin (theta) * Math.sin (rho);
    double z = Math.cos (rho);
    sphere_normal (x, y, z);    

    float vx = (float) (x * radius);
    float vy = (float) (y * radius);
    float vz = (float) (z * radius);
    gl.vertex3f (vx, vy, vz);
  }


  private void sphere_normal (double x, double y, double z) {
    if (normals == GLU.NONE) return;

    float vx = (float) (x * nsign);
    float vy = (float) (y * nsign);
    float vz = (float) (z * nsign);
    gl.normal3f (vx, vy, vz);
  }
}
