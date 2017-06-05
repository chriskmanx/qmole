package gnu.app.redbook;

import gnu.x11.extension.glx.GL;
import gnu.x11.extension.glx.GLU;


/**
 * Utility for anti-aliasing. Modified from <code>accpersp.h</code>.
*/
class AntiAlias {
  /** 
   * {@link GLU.frustum} with anti-aliasing.
   *
   * <p>Other arguments are identical to {@link GLU.frustum}. Set
   * both `pixdx' and `pixdy' equal to 0.0 for no anti-alias jitter. Set
   * both `eyex' and `eyey' equal to 0.0 for no depth of field effects.
   * 
   * @param pixdx x of anti-alias jitter in pixels
   * @param pixdy y of anti-alias jitter in pixels
   * @param eyedx x of depth-of field jitter in pixels
   * @param eyedy y of depth-of field jitter in pixels
   * @param focus distance from eye to plane in focus, must be greater
   * than, but not equal to 0.0
   */
  static void frustum (GL gl, int window_width, int window_height, 
    double left, double right, double bottom, double top, 
    double near, double far, double pixdx, double pixdy, 
    double eyedx, double eyedy, double focus) {

    double xwsize = right - left;
    double ywsize = top - bottom;
    double dx = pixdx*xwsize/window_width + eyedx*near/focus;
    double dy = pixdy*ywsize/window_height + eyedy*near/focus;

    gl.matrix_mode (GL.PROJECTION);
    gl.load_identity ();    
    gl.frustum (left-dx, right-dx, bottom-dy, top-dy, near, far);

    gl.matrix_mode (GL.MODELVIEW);
    gl.load_identity ();
    gl.translatef ((float) -eyedx, (float) -eyedy, 0.0f);
  }


  /** 
   * {@link GLU.perspective} with anti-aliasing.
   *
   * <p>Other arguments are identical to {@link GLU.perspective}. Set
   * both `pixdx' and `pixdy' equal to 0.0 for no anti-alias jitter. Set
   * both `eyex' and `eyey' equal to 0.0 for no depth of field effects.
   * 
   * @param pixdx x of anti-alias jitter in pixels
   * @param pixdy y of anti-alias jitter in pixels
   * @param eyedx x of depth-of field jitter in pixels
   * @param eyedy y of depth-of field jitter in pixels
   * @param focus distance from eye to plane in focus, must be greater
   * than, but not equal to 0.0
   */
  static void perspective (GL gl, int window_width, int window_height,
    double fovy, double aspect, double near, double far, 
    double pixdx, double pixdy, double eyedx, double eyedy, double focus) {

    // identical to code of `GLU.perspective'
    double ymax = near * Math.tan (Math.toRadians (fovy/2));
    double ymin = -ymax;
    double xmin = ymin * aspect;
    double xmax = ymax * aspect;

    frustum (gl, window_width, window_height, 
      xmin, xmax, ymin, ymax, near, far, 
      pixdx, pixdy, eyedx, eyedy, focus);
  }   
}
