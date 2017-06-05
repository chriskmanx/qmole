/**
 * orbit-policy.h: re-enterancy policy object for client invocations
 *
 * Author:
 *   Michael Meeks (michael@ximian.com)
 *
 * Copyright 2003 Ximian, Inc.
 */
#ifndef _ORBIT_POLICY_H_
#define _ORBIT_POLICY_H_

#include <orbit/orbit.h>

G_BEGIN_DECLS

struct _ORBitPolicy {
	struct ORBit_RootObject_struct parent;

	GPtrArray *allowed_poas;
};

gboolean ORBit_policy_validate (ORBitPolicy *policy);

G_END_DECLS

#endif /* _ORBIT_POLICY_H_ */
