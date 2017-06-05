#ifndef _ORBIT_INIT_H_
#define _ORBIT_INIT_H_

DynamicAny_DynAnyFactory
     ORBit_DynAnyFactory_new (CORBA_ORB orb, CORBA_Environment *ev);
void ORBit_init_internals    (CORBA_ORB orb, CORBA_Environment *ev);

#endif /* _ORBIT_INIT_H */
