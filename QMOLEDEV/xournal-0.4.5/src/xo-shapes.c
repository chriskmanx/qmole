#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <math.h>
#include <string.h>
#include <gtk/gtk.h>
#include <libgnomecanvas/libgnomecanvas.h>

#include "xournal.h"
#include "xo-shapes.h"
#include "xo-paint.h"

typedef struct Inertia {
  double mass, sx, sy, sxx, sxy, syy;
} Inertia;

typedef struct RecoSegment {
  struct Item *item;
  int startpt, endpt;
  double xcenter, ycenter, angle, radius;
  double x1, y1, x2, y2;
  gboolean reversed;
} RecoSegment;

struct RecoSegment recognizer_queue[MAX_POLYGON_SIDES+1];
int recognizer_queue_length;
struct UndoItem *last_item_checker; // check if queue is stale

void reset_recognizer(void)
{
  recognizer_queue_length = 0;
  last_item_checker = NULL;
}

/* compute mass and moments of a stroke */

void incr_inertia(double *pt, struct Inertia *s, int coef)
{
  double dm;
  dm = coef*hypot(pt[2]-pt[0], pt[3]-pt[1]);
  s->mass += dm;
  s->sx += dm*pt[0];
  s->sy += dm*pt[1];
  s->sxx += dm*pt[0]*pt[0];
  s->syy += dm*pt[1]*pt[1];
  s->sxy += dm*pt[0]*pt[1];
}
   
void calc_inertia(double *pt, int start, int end, struct Inertia *s)
{
  int i;
  
  s->mass = s->sx = s->sy = s->sxx = s->sxy = s->syy = 0.;
  for (i=start, pt+=2*start; i<end; i++, pt+=2) incr_inertia(pt, s, 1);
}

/* compute normalized quantities */

inline double center_x(struct Inertia s) 
{ 
  return s.sx/s.mass;
}

inline double center_y(struct Inertia s) 
{ 
  return s.sy/s.mass;
}

inline double I_xx(struct Inertia s)
{
  if (s.mass <= 0.) return 0.;
  return (s.sxx - s.sx*s.sx/s.mass)/s.mass;
}

inline double I_xy(struct Inertia s)
{
  if (s.mass <= 0.) return 0.;
  return (s.sxy - s.sx*s.sy/s.mass)/s.mass;
}

inline double I_yy(struct Inertia s)
{
  if (s.mass <= 0.) return 0.;
  return (s.syy - s.sy*s.sy/s.mass)/s.mass;
}

inline double I_rad(struct Inertia s)
{
  double ixx = I_xx(s), iyy = I_yy(s);
  if (ixx+iyy <= 0.) return 0.;
  return sqrt(ixx+iyy);
}

inline double I_det(struct Inertia s)
{
  double ixx = I_xx(s), iyy = I_yy(s), ixy = I_xy(s);
  if (s.mass <= 0.) return 0.;
  if (ixx+iyy <= 0.) return 0.;
  return 4*(ixx*iyy-ixy*ixy)/(ixx+iyy)/(ixx+iyy);
}

/* check if something is a polygonal line with at most nsides sides */

int find_polygonal(double *pt, int start, int end, int nsides, int *breaks, struct Inertia *ss)
{
  struct Inertia s, s1, s2;
  int k, i1, i2, n1, n2;
  double det1, det2;
  
  if (end == start) return 0; // no way
  if (nsides <= 0) return 0;
  if (end-start<5) nsides = 1; // too small for a polygon
  
  // look for a linear piece that's big enough
  for (k=0; k<nsides; k++) {
    i1 = start + (k*(end-start))/nsides; 
    i2 = start + ((k+1)*(end-start))/nsides;
    calc_inertia(pt, i1, i2, &s);
    if (I_det(s) < LINE_MAX_DET) break;
  }
  if (k==nsides) return 0; // failed!
  
  // grow the linear piece we found
  while (1) {
    if (i1>start) {
      s1 = s;
      incr_inertia(pt+2*(i1-1), &s1, 1);
      det1 = I_det(s1);
    } 
    else det1 = 1.;
    if (i2<end) {
      s2 = s;
      incr_inertia(pt+2*i2, &s2, 1);
      det2 = I_det(s2);
    }
    else det2 = 1.;
    if (det1<det2 && det1<LINE_MAX_DET) { i1--; s=s1; }
    else if (det2<det1 && det2<LINE_MAX_DET) { i2++; s=s2; }
    else break;
  }
  
  if (i1>start) {
    n1 = find_polygonal(pt, start, i1, (i2==end)?(nsides-1):(nsides-2), breaks, ss);
    if (n1 == 0) return 0; // it doesn't work
  }
  else n1 = 0;

  breaks[n1] = i1;
  breaks[n1+1] = i2;
  ss[n1] = s;

  if (i2<end) {
    n2 = find_polygonal(pt, i2, end, nsides-n1-1, breaks+n1+1, ss+n1+1);
    if (n2 == 0) return 0;
  }
  else n2 = 0;
  
  return n1+n2+1;
}

/* improve on the polygon found by find_polygonal() */

void optimize_polygonal(double *pt, int nsides, int *breaks, struct Inertia *ss)
{
  int i;
  double cost, newcost;
  struct Inertia s1, s2;
  gboolean improved;
  
  for (i=1; i<nsides; i++) {
    // optimize break between sides i and i+1
    cost = I_det(ss[i-1])*I_det(ss[i-1])+I_det(ss[i])*I_det(ss[i]);
    s1 = ss[i-1]; s2 = ss[i];
    improved = FALSE;
    while (breaks[i]>breaks[i-1]+1) {
      // try moving the break to the left
      incr_inertia(pt+2*(breaks[i]-1), &s1, -1);
      incr_inertia(pt+2*(breaks[i]-1), &s2, 1);
      newcost = I_det(s1)*I_det(s1)+I_det(s2)*I_det(s2);
      if (newcost >= cost) break;
      improved = TRUE;
      cost = newcost; 
      breaks[i]--;
      ss[i-1] = s1;
      ss[i] = s2;
    }
    if (improved) continue;
    s1 = ss[i-1]; s2 = ss[i];
    while (breaks[i]<breaks[i+1]-1) {
      // try moving the break to the right
      incr_inertia(pt+2*breaks[i], &s1, 1);
      incr_inertia(pt+2*breaks[i], &s2, -1);
      newcost = I_det(s1)*I_det(s1)+I_det(s2)*I_det(s2);
      if (newcost >= cost) break;
      cost = newcost; 
      breaks[i]++;
      ss[i-1] = s1;
      ss[i] = s2;
    }
  }
}

/* find the geometry of a recognized segment */

void get_segment_geometry(double *pt, int start, int end, struct Inertia *s, struct RecoSegment *r)
{
  double a, b, c, lmin, lmax, l;
  int i;
  
  r->xcenter = center_x(*s);
  r->ycenter = center_y(*s);
  a = I_xx(*s); b = I_xy(*s); c = I_yy(*s);
  /* max angle for inertia quadratic form solves: tan(2t) = 2b/(a-c) */
  r->angle = atan2(2*b, a-c)/2; 
  r->radius = sqrt(3*(a+c));
  
  lmin = lmax = 0.;
  for (i=start, pt+=2*start; i<=end; i++, pt+=2) {
    l = (pt[0]-r->xcenter)*cos(r->angle)+(pt[1]-r->ycenter)*sin(r->angle);
    if (l<lmin) lmin = l;
    if (l>lmax) lmax = l;
  }
  r->x1 = r->xcenter + lmin*cos(r->angle);
  r->y1 = r->ycenter + lmin*sin(r->angle);
  r->x2 = r->xcenter + lmax*cos(r->angle);
  r->y2 = r->ycenter + lmax*sin(r->angle);
}

/* test if we have a circle; inertia has been precomputed by caller */

double score_circle(double *pt, int start, int end, struct Inertia *s)
{
  double sum, x0, y0, r0, dm, deltar;
  int i;
  
  if (s->mass == 0.) return 0;
  sum = 0.;
  x0 = center_x(*s); y0 = center_y(*s); r0 = I_rad(*s);
  for (i=start, pt+=2*start; i<end; i++, pt+=2) {
    dm = hypot(pt[2]-pt[0], pt[3]-pt[1]);
    deltar = hypot(pt[0]-x0, pt[1]-y0) - r0;
    sum += dm * fabs(deltar);
  }
  return sum/(s->mass*r0);
}

/* replace strokes by various shapes */

void make_circle_shape(double x0, double y0, double r)
{
  int npts, i;
  struct Item *item;
  struct UndoErasureData *erasure;
  
  npts = (int)(2*r);
  if (npts<12) npts = 12; // min. number of points
  realloc_cur_path(npts+1);
  ui.cur_path.num_points = npts+1;
  for (i=0;i<=npts; i++) {
    ui.cur_path.coords[2*i] = x0 + r*cos((2*M_PI*i)/npts);
    ui.cur_path.coords[2*i+1] = y0 + r*sin((2*M_PI*i)/npts);
  }
}

void calc_edge_isect(struct RecoSegment *r1, struct RecoSegment *r2, double *pt)
{
  double t;
  t = (r2->xcenter - r1->xcenter) * sin(r2->angle) - 
      (r2->ycenter - r1->ycenter) * cos(r2->angle);
  t /= sin(r2->angle-r1->angle);
  pt[0] = r1->xcenter + t*cos(r1->angle);
  pt[1] = r1->ycenter + t*sin(r1->angle);
}

void remove_recognized_strokes(struct RecoSegment *rs, int num_old_items)
{
  struct Item *old_item;
  int i, shift;
  struct UndoErasureData *erasure;

  old_item = NULL;
  prepare_new_undo();
  undo->type = ITEM_RECOGNIZER;
  undo->layer = ui.cur_layer;
  undo->erasurelist = NULL;
  shift = 0;
  
  for (i=0; i<num_old_items; i++) {
    if (rs[i].item == old_item) continue; // already done
    old_item = rs[i].item;
    erasure = g_new(struct UndoErasureData, 1);
    erasure->item = old_item;
    erasure->npos = g_list_index(ui.cur_layer->items, old_item) + (shift++);
    erasure->nrepl = 0;
    erasure->replacement_items = NULL;
    undo->erasurelist = g_list_append(undo->erasurelist, erasure);
    if (old_item->canvas_item != NULL)
      gtk_object_destroy(GTK_OBJECT(old_item->canvas_item));
    ui.cur_layer->items = g_list_remove(ui.cur_layer->items, old_item);
    ui.cur_layer->nitems--;
  }
}

struct Item *insert_recognized_curpath(void)
{
  struct Item *item;
  int i;
  struct UndoErasureData *erasure;

  erasure = (struct UndoErasureData *)(undo->erasurelist->data);
  item = g_new(struct Item, 1);
  item->type = ITEM_STROKE;
  g_memmove(&(item->brush), &(erasure->item->brush), sizeof(struct Brush));
  item->brush.variable_width = FALSE;
  subdivide_cur_path();
  item->path = gnome_canvas_points_new(ui.cur_path.num_points);
  g_memmove(item->path->coords, ui.cur_path.coords, 2*ui.cur_path.num_points*sizeof(double));
  item->widths = NULL;
  update_item_bbox(item);
  ui.cur_path.num_points = 0;
  
  erasure->nrepl++;
  erasure->replacement_items = g_list_append(erasure->replacement_items, item);
  ui.cur_layer->items = g_list_append(ui.cur_layer->items, item);
  ui.cur_layer->nitems++;
  make_canvas_item_one(ui.cur_layer->group, item);
  return item;
}


/* test if segments form standard shapes */

gboolean try_rectangle(void)
{
  struct RecoSegment *rs, *r1, *r2;
  int i;
  double dist, avg_angle;
  
  // first, we need whole strokes to combine to 4 segments...
  if (recognizer_queue_length<4) return FALSE;
  rs = recognizer_queue + recognizer_queue_length - 4;
  if (rs->startpt!=0) return FALSE;

  // check edges make angles ~= Pi/2 and vertices roughly match
  avg_angle = 0.;
  for (i=0; i<=3; i++) {
    r1 = rs+i; r2 = rs+(i+1)%4;
    if (fabs(fabs(r1->angle-r2->angle)-M_PI/2) > RECTANGLE_ANGLE_TOLERANCE)
      return FALSE;
    avg_angle += r1->angle;
    if (r2->angle > r1->angle) avg_angle += (i+1)*M_PI/2;
    else avg_angle -= (i+1)*M_PI/2;
    // test if r1 points away from r2 rather than towards it
    r1->reversed = ((r1->x2-r1->x1)*(r2->xcenter-r1->xcenter)+
                   (r1->y2-r1->y1)*(r2->ycenter-r1->ycenter)) < 0;
  }
  for (i=0; i<=3; i++) {
    r1 = rs+i; r2 = rs+(i+1)%4;
    dist = hypot((r1->reversed?r1->x1:r1->x2) - (r2->reversed?r2->x2:r2->x1),
                 (r1->reversed?r1->y1:r1->y2) - (r2->reversed?r2->y2:r2->y1));
    if (dist > RECTANGLE_LINEAR_TOLERANCE*(r1->radius+r2->radius)) return FALSE;
  }
  
  // make a rectangle of the correct size and slope
  avg_angle = avg_angle/4;
  if (fabs(avg_angle)<SLANT_TOLERANCE) avg_angle = 0.;
  if (fabs(avg_angle)>M_PI/2-SLANT_TOLERANCE) avg_angle = M_PI/2;
  realloc_cur_path(5);
  ui.cur_path.num_points = 5;
  for (i=0; i<=3; i++) rs[i].angle = avg_angle+i*M_PI/2;
  for (i=0; i<=3; i++) calc_edge_isect(rs+i, rs+(i+1)%4, ui.cur_path.coords+2*i+2);
  ui.cur_path.coords[0] = ui.cur_path.coords[8];
  ui.cur_path.coords[1] = ui.cur_path.coords[9];
  
  remove_recognized_strokes(rs, 4);
  insert_recognized_curpath();
  return TRUE;
}

gboolean try_arrow(void)
{
  struct RecoSegment *rs;
  int i, j;
  double alpha[3], dist, pt[2], tmp, delta;
  double x1, y1, x2, y2, angle;
  gboolean rev[3];
  
  // first, we need whole strokes to combine to nsides segments...
  if (recognizer_queue_length<3) return FALSE;
  rs = recognizer_queue + recognizer_queue_length - 3;
  if (rs->startpt!=0) return FALSE;

  // check arrow head not too big, and orient main segment
  for (i=1; i<=2; i++) {
    if (rs[i].radius > ARROW_MAXSIZE*rs[0].radius) return FALSE;
    rev[i] = (hypot(rs[i].xcenter-rs->x1, rs[i].ycenter-rs->y1) <
              hypot(rs[i].xcenter-rs->x2, rs[i].ycenter-rs->y2));
  }
  if (rev[1]!=rev[2]) return FALSE;
  if (rev[1]) { 
    x1 = rs->x2; y1 = rs->y2; x2 = rs->x1; y2 = rs->y1; 
    angle = rs->angle + M_PI;
  }
  else { 
    x1 = rs->x1; y1 = rs->y1; x2 = rs->x2; y2 = rs->y2;
    angle = rs->angle;
  }
    
  // check arrow head not too big, and angles roughly ok
  for (i=1; i<=2; i++) {
    rs[i].reversed = FALSE;
    alpha[i] = rs[i].angle - angle;
    while (alpha[i]<-M_PI/2) { alpha[i]+=M_PI; rs[i].reversed = !rs[i].reversed; }
    while (alpha[i]>M_PI/2) { alpha[i]-=M_PI; rs[i].reversed = !rs[i].reversed; }
#ifdef RECOGNIZER_DEBUG
    printf("DEBUG: arrow: alpha[%d] = %.1f degrees\n", i, alpha[i]*180/M_PI);
#endif
    if (fabs(alpha[i])<ARROW_ANGLE_MIN || fabs(alpha[i])>ARROW_ANGLE_MAX) return FALSE;
  }
  
  // check arrow head segments are roughly symmetric
  if (alpha[1]*alpha[2]>0 || fabs(alpha[1]+alpha[2]) > ARROW_ASYMMETRY_MAX_ANGLE) return FALSE;
  if (rs[1].radius/rs[2].radius > 1+ARROW_ASYMMETRY_MAX_LINEAR) return FALSE;
  if (rs[2].radius/rs[1].radius > 1+ARROW_ASYMMETRY_MAX_LINEAR) return FALSE;

  // check vertices roughly match
  calc_edge_isect(rs+1, rs+2, pt);
  for (j=1; j<=2; j++) {
    dist = hypot(pt[0]-(rs[j].reversed?rs[j].x1:rs[j].x2),
                 pt[1]-(rs[j].reversed?rs[j].y1:rs[j].y2));
#ifdef RECOGNIZER_DEBUG
    printf("DEBUG: linear tolerance: tip[%d] = %.2f\n", j, dist/rs[j].radius);
#endif
    if (dist>ARROW_TIP_LINEAR_TOLERANCE*rs[j].radius) return FALSE;
  }
  dist = (pt[0]-x2)*sin(angle)-(pt[1]-y2)*cos(angle);
  dist /= rs[1].radius + rs[2].radius;
#ifdef RECOGNIZER_DEBUG
  printf("DEBUG: sideways gap tolerance = %.2f\n", dist);
#endif
  if (fabs(dist)>ARROW_SIDEWAYS_GAP_TOLERANCE) return FALSE;
  dist = (pt[0]-x2)*cos(angle)+(pt[1]-y2)*sin(angle);
  dist /= rs[1].radius + rs[2].radius;
#ifdef RECOGNIZER_DEBUG
  printf("DEBUG: main linear gap = %.2f\n", dist);
#endif
  if (dist<ARROW_MAIN_LINEAR_GAP_MIN || dist>ARROW_MAIN_LINEAR_GAP_MAX) return FALSE;

  // make an arrow of the correct size and slope
  if (fabs(rs->angle)<SLANT_TOLERANCE) { // nearly horizontal
    angle = angle - rs->angle;
    y1 = y2 = rs->ycenter;
  }
  if (rs->angle>M_PI/2-SLANT_TOLERANCE) { // nearly vertical
    angle = angle - (rs->angle-M_PI/2);
    x1 = x2 = rs->xcenter;
  }
  if (rs->angle<-M_PI/2+SLANT_TOLERANCE) { // nearly vertical
    angle = angle - (rs->angle+M_PI/2);
    x1 = x2 = rs->xcenter;
  }
  delta = fabs(alpha[1]-alpha[2])/2;
  dist = (hypot(rs[1].x1-rs[1].x2, rs[1].y1-rs[1].y2) +
          hypot(rs[2].x1-rs[2].x2, rs[2].y1-rs[2].y2))/2;
  
  realloc_cur_path(2);
  ui.cur_path.num_points = 2;
  ui.cur_path.coords[0] = x1; ui.cur_path.coords[1] = y1;
  ui.cur_path.coords[2] = x2; ui.cur_path.coords[3] = y2;
  remove_recognized_strokes(rs, 3);
  insert_recognized_curpath();

  realloc_cur_path(3);
  ui.cur_path.num_points = 3;
  ui.cur_path.coords[0] = x2 - dist*cos(angle+delta);
  ui.cur_path.coords[1] = y2 - dist*sin(angle+delta);
  ui.cur_path.coords[2] = x2; 
  ui.cur_path.coords[3] = y2;
  ui.cur_path.coords[4] = x2 - dist*cos(angle-delta);
  ui.cur_path.coords[5] = y2 - dist*sin(angle-delta);
  insert_recognized_curpath();

  return TRUE;
}

gboolean try_closed_polygon(int nsides)
{
  struct RecoSegment *rs, *r1, *r2;
  int i;
  double dist, pt[2];
  
  // first, we need whole strokes to combine to nsides segments...
  if (recognizer_queue_length<nsides) return FALSE;
  rs = recognizer_queue + recognizer_queue_length - nsides;
  if (rs->startpt!=0) return FALSE;

  // check vertices roughly match
  for (i=0; i<nsides; i++) {
    r1 = rs+i; r2 = rs+(i+1)%nsides;
    // test if r1 points away from r2 rather than towards it
    calc_edge_isect(r1, r2, pt);
    r1->reversed = (hypot(pt[0]-r1->x1,pt[1]-r1->y1) < hypot(pt[0]-r1->x2,pt[1]-r1->y2));
  }
  for (i=0; i<nsides; i++) {
    r1 = rs+i; r2 = rs+(i+1)%nsides;
    calc_edge_isect(r1, r2, pt);
    dist = hypot((r1->reversed?r1->x1:r1->x2)-pt[0],(r1->reversed?r1->y1:r1->y2)-pt[1])
         + hypot((r2->reversed?r2->x2:r2->x1)-pt[0],(r2->reversed?r2->y2:r2->y1)-pt[1]);
    if (dist > POLYGON_LINEAR_TOLERANCE*(r1->radius+r2->radius)) return FALSE;
  }
  
  // make a polygon of the correct size and slope
  realloc_cur_path(nsides+1);
  ui.cur_path.num_points = nsides+1;
  for (i=0; i<nsides; i++) 
    calc_edge_isect(rs+i, rs+(i+1)%nsides, ui.cur_path.coords+2*i+2);
  ui.cur_path.coords[0] = ui.cur_path.coords[2*nsides];
  ui.cur_path.coords[1] = ui.cur_path.coords[2*nsides+1];
  
  remove_recognized_strokes(rs, nsides);
  insert_recognized_curpath();
  return TRUE;
}

/* the main pattern recognition function, called after finalize_stroke() */
void recognize_patterns(void)
{
  struct Item *it;
  struct Inertia s, ss[4];
  struct RecoSegment *rs;
  int n, i;
  int brk[5];
  double score;
  
  if (!undo || undo->type!=ITEM_STROKE) return;
  if (undo->next != last_item_checker) reset_recognizer(); // reset queue
  if (last_item_checker!=NULL && ui.cur_layer != last_item_checker->layer) reset_recognizer();

  it = undo->item;
  calc_inertia(it->path->coords, 0, it->path->num_points-1, &s);
#ifdef RECOGNIZER_DEBUG
  printf("DEBUG: Mass=%.0f, Center=(%.1f,%.1f), I=(%.0f,%.0f, %.0f), "
     "Rad=%.2f, Det=%.4f \n", 
     s.mass, center_x(s), center_y(s), I_xx(s), I_yy(s), I_xy(s), I_rad(s), I_det(s));
#endif

  // first see if it's a polygon
  n = find_polygonal(it->path->coords, 0, it->path->num_points-1, MAX_POLYGON_SIDES, brk, ss);
  if (n>0) {
    optimize_polygonal(it->path->coords, n, brk, ss);
#ifdef RECOGNIZER_DEBUG
    printf("DEBUG: Polygon, %d edges: ", n);
    for (i=0; i<n; i++)
      printf("DEBUG: %d-%d (M=%.0f, det=%.4f) ", brk[i], brk[i+1], ss[i].mass, I_det(ss[i]));
    printf("\n");
#endif
    /* update recognizer segment queue (most recent at end) */
    while (n+recognizer_queue_length > MAX_POLYGON_SIDES) {
      // remove oldest polygonal stroke
      i=1;
      while (i<recognizer_queue_length && recognizer_queue[i].startpt!=0) i++;
      recognizer_queue_length-=i;
      g_memmove(recognizer_queue, recognizer_queue+i, 
              recognizer_queue_length * sizeof(struct RecoSegment));
    }
#ifdef RECOGNIZER_DEBUG
    printf("DEBUG: Queue now has %d + %d edges\n", recognizer_queue_length, n);
#endif
    rs = recognizer_queue + recognizer_queue_length;
    recognizer_queue_length += n;
    for (i=0; i<n; i++) {
      rs[i].item = it;
      rs[i].startpt = brk[i];
      rs[i].endpt = brk[i+1];
      get_segment_geometry(it->path->coords, brk[i], brk[i+1], ss+i, rs+i);
    }  
    if (try_rectangle()) { reset_recognizer(); return; }
    if (try_arrow()) { reset_recognizer(); return; }
    if (try_closed_polygon(3)) { reset_recognizer(); return; }
    if (try_closed_polygon(4)) { reset_recognizer(); return; }
    if (n==1) { // current stroke is a line
      if (fabs(rs->angle)<SLANT_TOLERANCE) { // nearly horizontal
        rs->angle = 0.;
        rs->y1 = rs->y2 = rs->ycenter;
      }
      if (fabs(rs->angle)>M_PI/2-SLANT_TOLERANCE) { // nearly vertical
        rs->angle = (rs->angle>0)?(M_PI/2):(-M_PI/2);
        rs->x1 = rs->x2 = rs->xcenter;
      }
      realloc_cur_path(2);
      ui.cur_path.num_points = 2;
      ui.cur_path.coords[0] = rs->x1;
      ui.cur_path.coords[1] = rs->y1;
      ui.cur_path.coords[2] = rs->x2;
      ui.cur_path.coords[3] = rs->y2;
      remove_recognized_strokes(rs, 1);
      rs->item = insert_recognized_curpath();
    }
    last_item_checker = undo;
    return;
  }

  // not a polygon: maybe a circle ?
  reset_recognizer();
  if (I_det(s)>CIRCLE_MIN_DET) {
    score = score_circle(it->path->coords, 0, it->path->num_points-1, &s);
#ifdef RECOGNIZER_DEBUG
    printf("DEBUG: Circle score: %.2f\n", score);
#endif
    if (score < CIRCLE_MAX_SCORE) {
      make_circle_shape(center_x(s), center_y(s), I_rad(s));
      recognizer_queue[0].item = it;
      remove_recognized_strokes(recognizer_queue, 1);
      insert_recognized_curpath();
    }
  }
}

