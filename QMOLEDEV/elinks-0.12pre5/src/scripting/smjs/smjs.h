#ifndef EL__SCRIPTING_SMJS_SMJS_H
#define EL__SCRIPTING_SMJS_SMJS_H

struct module;
struct cache_entry;

extern struct module smjs_scripting_module;

/* TODO: Use trigger_event() for this, so that the caching code need
 * not directly call the SMJS scripting module?  That would require
 * changes in struct cache_object though, to let a dynamic number of
 * scripting modules have pointers from there to their objects.  */
void smjs_detach_cache_entry_object(struct cache_entry *cached);

#endif
