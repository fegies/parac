class a
{
	left;
	right;
	value;
}

function serialize( a )
{
	if( a == 0); then
		return;
	fi
	serialize( a.left );
	print( a.value );
	serialize( a.right );
}

function p( a )
{
	for i <- 0 to a; do
		print(i);
	od
}

i <- 0;

while 1; do
{
	i++;
	p( i );
}od
