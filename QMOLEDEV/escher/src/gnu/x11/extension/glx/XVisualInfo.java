/* 
 * NOTE
 * ----
 * 
 * This class contains code borrowed, or derive from the MESA project
 * released under a MIT license. 
 * 
 * Copyright (C) 1999-2007  Brian Paul   All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL BRIAN PAUL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package gnu.x11.extension.glx;

import gnu.x11.Data;
import gnu.x11.ResponseInputStream;

/**
 * GLX visual configuration. A single display can support multiple screens. Each
 * screen can have several different visual types supported at different depths.
 */
public class XVisualInfo {
    
    // NOTE: in theory this class should extend the VisualInfo class
    // We also have few other "visual" classes, and they are all related.
    // The reason for this screwed design is that there is no clear
    // documentation about all this stuff, they are just randomly
    // referenced any here and there in the various GLX and X11 docs,
    // so we had no clue on what to do before hitting some problems
    // and working on them.
    // I feel really sorry for the design, but also quite disappointed, indeed,
    // by the bad docs.
    
    private int count = 0;
    
    /** Visual ID */
    private int id = (int) GLXConstants.GLX_DONT_CARE;
    
    /** One of the GLX X visual types, like GLX_TRUE_COLOR */
    private int visualType = (int) GLXConstants.GLX_DONT_CARE;
   
    private boolean rgbMode = false; 
    private boolean floatMode = false;
    private boolean colorIndexMode = false;
    
    private long stereoMode = GLXConstants.GLX_DONT_CARE;
    private long doubleBufferMode;
    
    private boolean haveAccumBuffer = false;
    private boolean haveDepthBuffer = false;
    private boolean haveStencilBuffer = false;
    
    /** Bits per component */
    private int redBits = 0, greenBits = 0, blueBits = 0, alphaBits = 0;
    private long redMask = 0, greenMask = 0, blueMask = 0, alphaMask = 0;
    
    /** Total bits for rgb */
    private int rgbBits;
    
    /** Total bits for colorindex  */
    private int indexBits;
    
    /** Bits for accumulation buffer. */
    private int accumRedBits = 0, accumGreenBits = 0, accumBlueBits = 0,
                accumAlphaBits = 0;
    
    private int depthBits = 0;   
    private int stencilBits = 0;
    
    private int numAuxBuffers = 0;
    
    private int level = 0;
    
    private int pixmapMode = 0;
   
    /* ***** Please, try to keep the extensions ordered by GLX version. ***** */
    
    /** EXT_visual_rating / GLX 1.2 extension */
    private int visualRating = (int) GLXConstants.GLX_DONT_CARE;
    
    /* EXT_visual_info / GLX 1.2 */
    private int transparentPixel = (int) GLXConstants.GLX_DONT_CARE;;
    private int transparentRed = 0, transparentGreen = 0, transparentBlue = 0,
                transparentAlpha = 0;
    private int transparentIndex = 0;
    
    /* ARB_multisample / SGIS_multisample */
    private int sampleBuffers = 0;
    private int samples = 0;

    /** SGIX_fbconfig / GLX 1.3 */
    private boolean isFBConfig = false;
    private int drawableType = GLXConstants.GLX_WINDOW_BIT;
    private int renderType = 0;
    private int xRenderable = (int) GLXConstants.GLX_DONT_CARE;
    private int fbconfigID = (int) GLXConstants.GLX_DONT_CARE;

    /** SGIX_pbuffer / GLX 1.3 */
    private int maxPbufferWidth = 0;
    private int maxPbufferHeight = 0;
    private int maxPbufferPixels = 0;
    private int optimalPbufferWidth = 0;   // Only for SGIX_pbuffer.
    private int optimalPbufferHeight = 0;  // Only for SGIX_pbuffer
    
    /** SGIX_visual_select_group  */
    private int visualSelectGroup = 0;
    
    /** OML_swap_method */
    private int swapMethod = (int) GLXConstants.GLX_DONT_CARE;
    private int screen = 0;
    
    /** EXT_texture_from_pixmap */
    private int bindToTextureRgb = 0;
    private int bindToTextureRgba = 0;
    private int bindToMipmapTexture = 0;
    private int bindToTextureTargets = 0;
    private int yInverted = 0;

//    /**
//     * Initialize an XVisualInfo class directly from the ResponseInputStream.
//     */
//    public XVisualInfo(ResponseInputStream stream, int count) {
//        
//        this.count = count;
//        
//        // read properties from the stream
//        this.id = stream.read_int32();
//    }
    
    /**
     * Initialize a default, no FBConfig type 
     */
    public XVisualInfo() {
        
        this(false);
    }

    public XVisualInfo(boolean isFBConfig) {
        
        this.isFBConfig = isFBConfig;
        if (this.isFBConfig) {
            this.rgbMode = true;
            this.doubleBufferMode = GLXConstants.GLX_DONT_CARE;
        }
        
        this.renderType = this.rgbMode ? GLXConstants.GLX_RGBA_BIT :
                                         GLXConstants.GLX_COLOR_INDEX_BIT;
    }
    
    void setID(int id) {
        this.id = id;
    }
    
    public int getID() {
        
        return this.id;
    }
}
