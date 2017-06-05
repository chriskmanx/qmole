#ifndef ORB_CORE_EXPORT_H
#define ORB_CORE_EXPORT_H

ORBitSmallSkeleton get_small_skel_CORBA_Object (PortableServer_Servant  servant, 
						const char             *opname,
						gpointer               *m_data, 
						gpointer               *impl);

#endif /* ORB_CORE_EXPORT_H */
