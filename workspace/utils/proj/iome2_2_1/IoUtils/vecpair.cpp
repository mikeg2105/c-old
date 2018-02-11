// vecpair.cpp: implementation of the vecpair class.
//
//////////////////////////////////////////////////////////////////////
/*
IOME LICENSE
IOME Version 1.1.1

IOME Development  Tools
Copyright (C) 2001-2004, Michael Kenneth Griffiths, All Rights Reserved.

--------------------------------------------------------------------------------
IOME public license.

The contents of this file are subject to the IOME Public License Version 1.3
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://81.174.178.112/iome/licensing/iomelicense.html
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Michael Kenneth Griffiths.
Copyright (C) 2000-2004 Michael Kenneth Griffiths. All Rights Reserved.
--------------------------------------------------------------------------------
GPL license.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program; if not, write to the Free Software Foundation, Inc., 59 Temple
Place, Suite 330, Boston, MA 02111-1307 USA

Author contact information:
mikeg@photon0.freeserve.co.uk
--------------------------------------------------------------------------------
*/
	
//#include "stdafx.h"
//#include "simulantv2.h"
#include "vecpair.h"

//#ifdef _DEBUG
//#undef THIS_FILE
//static char THIS_FILE[]=__FILE__;
//#define new DEBUG_NEW
//#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////




////////////////////////////////////////////////////////////////////////
// vecpair member functions
// constructor
vecpair::vecpair(int n, int p)
{
	a=new vec(n); b=new vec(p);
}

vecpair::vecpair(vec& A, vec& B)
{
	a=new vec(A.length());
	*a=A;
	b=new vec(B.length());
	*b=B;
}

vecpair::vecpair(const vecpair &AB)
{
	*this=vecpair(*(AB.a),*(AB.b));
}

vecpair::~vecpair()
{
	delete [] a; delete [] b;
} //destructor

vecpair& vecpair::operator=(const vecpair& v1)
{
	*a= *(v1.a);

	*b= *(v1.b);
	return *this;

}

vecpair& vecpair::scale(vecpair& minvecs, vecpair& maxvecs)
{	
	a->scale(*(minvecs.a), *(maxvecs.a));
	b->scale(*(minvecs.b), *(maxvecs.b));
	return *this;
}

int vecpair::operator==(const vecpair& v1)
{
	return (*a == *(v1.a)) && (*b == *(v1.b));
	
}

istream& operator>>(istream& s, vecpair &v1)
// input a vector pair
{
	v1.a->read(s, *v1.a);
	v1.b->read(s, *v1.b);
	
	//s>> *(v1.a) >> *(v1.b);
	return s;
}

ostream& operator<<(ostream& s, vecpair &v1)
// print a vector pair
{

    v1.a->write(s, *v1.a);
	v1.b->write(s, *v1.b);

	s<<"\n";
	return s;
	//return s<< *(v1.a) << *(v1.b) << "\n";
}

