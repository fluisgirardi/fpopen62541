#!/bin/bash
if [ $# -ne 2 ] ; then
  echo "$0 <open62541-source-directory> <destination directory>"
  exit 100
fi

opdir=$(readlink -f "$1/tools/")
if [ ! -d "$opdir" ] ; then
  echo "$opdir doesn't exist"
  exit 100
fi

dstdir=$(readlink -f "$2")
if [ ! -d "$dstdir" ] ; then
  echo "$dstdir doesn't exist"
  exit 100
fi

script="$(readlink -f "$0")"
srcdir=$(dirname "$script")


cp -f "${srcdir}/generate_datatypes_fpc.py" "${opdir}/"
cp -f "${srcdir}/backend_open62541_typedefinitions_fpc.py" "${opdir}/nodeset_compiler/"


pushd "${opdir}"
"${srcdir}/generate_statuscode_fpc.py"  schema/StatusCode.csv "${dstdir}/statuscodes"
"${srcdir}/generate_nodeid_header_fpc.py"  schema/NodeIds.csv "${dstdir}/nodeids" "NS0"

./generate_datatypes_fpc.py --selected-types=schema/datatypes_minimal.txt \
                                                   --selected-types=schema/datatypes_method.txt \
                                                   --selected-types=schema/datatypes_subscriptions.txt \
                                                   --selected-types=schema/datatypes_dataaccess.txt \
                                                   --selected-types=schema/datatypes_typedescription.txt \
                                                   --type-bsd=schema/Opc.Ua.Types.bsd \
                                                   --type-csv=schema/NodeIds.csv \
                                                   "${dstdir}/types"
popd                                                   
