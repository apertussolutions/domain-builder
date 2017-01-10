SPEC_TOOL=$(TOP)/bin/protocol-spec

# $1 Protocol name, the spec is:  protocols/$1.h
# $2 Include directroy
# $3 Generated source directory
#
# Example:
#
# $(eval $(call MAKE_PROTOCOL,control-domain-api,include,protocols))
#
# This uses $(TOP)/protocols/control-domain-api.h  as a specification.
#
# The generated headers look like this:
#     include/control-domain-api/types.h
#     include/control-domain-api/encode.h
#     etc.
#
# The genereated implementation look likt this:
#     protocols/control-domain-api_encode.c
#     protocols/control-domain-api_decode.c

define MAKE_PROTOCOL
$2/$1/encode.h $3/$1_encode.c: $$(TOP)/protocols/$1.h
	$Q mkdir -p $2/$1
	$Q mkdir -p $3
	$$(call cmd,SPEC_TOOL) $$< $1/types.h -i $2 --eh=$1/encode.h --ei=$3/$1_encode.c

$2/$1/decode.h $3/$1_decode.c: $$(TOP)/protocols/$1.h
	$Q mkdir -p $2/$1
	$Q mkdir -p $3
	$$(call cmd,SPEC_TOOL) $$< $1/types.h -i $2 --dh=$1/decode.h --di=$3/$1_decode.c

$2/$1/pretty.h $3/$1_pretty.c: $$(TOP)/protocols/$1.h
	$Q mkdir -p $2/$1
	$Q mkdir -p $3
	$$(call cmd,SPEC_TOOL) $$< $1/types.h -i $2 --ph=$1/pretty.h --pi=$3/$1_pretty.c
endef




