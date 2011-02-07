#include <stdlib.h> 
#include <JavaScriptCore/JSObjectRef.h>

#ifndef _JSCORE_ALIGNMENT
#define  _JSCORE_ALIGNMENT

#define ALIGNOF(type) offsetof (struct { char c; type member; }, member)


//int JSStaticValueAlign     (void);
//int JSStaticFunctionAlign  (void);
int JSClassDefinitionAlign (void);

#endif
