import re
import itertools as it
import collections
from sys import exit
from operator import itemgetter
import copy
from math import sqrt

def group_by(li, n):
    return zip(*[iter(li)]*n)

class Forest:
    def __init__(self, edges, atoms):
        self.edges    = edges
        self.atoms    = atoms

    def __str__(self):
        return "\n".join((
            'E = ' + str(self.edges),
            'A = ' + str(self.atoms)))

    def _edges_without_hydrogen(self):
        return [p for p in self.edges
            if (self.atoms[p[0]].upper() != 'H'
                and self.atoms[p[1]].upper() != 'H')]

    def _edges_with_hydrogen(self):
        return [p for p in self.edges
            if (self.atoms[p[0]].upper() == 'H'
                or self.atoms[p[1]].upper() == 'H')]

    def _grouped_edges(self, edges):
        grouped = [[] for _ in self.atoms]
        for src, dst in edges:
            grouped[src].append(dst)
            grouped[dst].append(src)
        return grouped

class ForestMatcher:
    @staticmethod
    def match(child, parent):
        return ForestMatcher(child, parent).match

    def __init__(self, child, parent):
        self.child = child
        self.parent = parent
        self.match = self._match_backbone()

    def _match_backbone(self):
        child = self.child
        parent = self.parent
        needle = child._edges_without_hydrogen()
        needle = [tuple(sorted(n)) for n in needle]
        needle = sorted(needle, key=itemgetter(0))
        haystack = parent._grouped_edges(
            parent._edges_without_hydrogen())
        #print(haystack)
        child_atoms = child.atoms
        parent_atoms = parent.atoms

        match = {}
        for i, atom in enumerate(child.atoms):
            if atom != 'H':
                match[i] = None
        n_used_edge = 0
        for i, atom in enumerate(child.atoms):
            if i in match and match[i] is not None:
                continue
            child_root = i
            for parent_root in range(len(parent.atoms)):
                # already used
                if parent_root in match.values():
                    continue
                # different kind of atoms
                if parent.atoms[parent_root] != child.atoms[child_root]:
                    continue
                old_match = match.copy()
                old_match[child_root] = parent_root
                new_n_used_edge, new_match = self._dfs(child_root, parent_root,
                    needle, haystack, n_used_edge, old_match)
                # print(new_match)
                # print("child_root, parent_root, ", child_root, parent_root)
                new_match = self._match_hydrogens(new_match)
                if not new_match is None:
                    n_used_edge = new_n_used_edge
                    match = new_match
        return match

    def _dfs(self, child_root, parent_root, needle, haystack,
        n_used_edge, old_match):
        child_atoms = self.child.atoms
        parent_atoms = self.parent.atoms
        stack = [(n_used_edge, old_match)]
        # print(child_root, parent_root)
        # print(needle)
        while stack:
            # Since molecule is not a tree, nbonds != natom - 1.
            # Therefore, the number of processed edge is
            # counted as n_used_edge.
            n_used_edge, match = stack.pop()
            # finished maching
            if n_used_edge == len(needle):
                return n_used_edge, match
            next_needle = needle[n_used_edge]
            next_needle_src, next_needle_dst = next_needle
            next_atom = child_atoms[next_needle_dst]
            # print(match)
            # print(next_needle_src)
            next_haystack_src = match[next_needle_src]
            next_haystack_dst = match[next_needle_dst]
            # finished making current molecule and looking next one
            if next_haystack_src is None:
                return n_used_edge, match
            # already found
            if next_haystack_dst is not None:
                if next_haystack_dst in haystack[next_haystack_src]:
                    stack.append((n_used_edge + 1, match))
                continue
            # newly found
            # Reverse-ordered loop makes correct-ordered stack.
            for next_haystack_dst in haystack[next_haystack_src][::-1]:
                if parent_atoms[next_haystack_dst] != next_atom:
                    continue
                if next_haystack_dst in match.values():
                    continue
                next_match = match.copy()
                next_match[next_needle_dst] = next_haystack_dst
                stack.append((n_used_edge + 1, next_match))
        return None, None

    def _match_hydrogens(self, match_backbone):
        child = self.child
        parent = self.parent
        child_edges = child._grouped_edges(child.edges)
        parent_edges = parent._grouped_edges(parent.edges)
        child_used = set(match_backbone.keys())
        parent_used = set(match_backbone.values())
        match_full = match_backbone.copy()
        # print(match_backbone)
        for child_atom, parent_atom in match_backbone.items():
            # unvisited
            if parent_atom is None:
                continue
            child_unused_edges = [x for x in child_edges[child_atom]
                if (x not in child_used)]
            parent_unused_edges = [x for x in parent_edges[parent_atom]
                if (x not in parent_used)]
            if len(child_unused_edges) != len(parent_unused_edges):
                return None
            for child_leaf, parent_leaf in \
                zip(child_unused_edges, parent_unused_edges):
                match_full[child_leaf] = parent_leaf
        return match_full
                
class UpdatePrmtop:
    def __init__(self, matchdict, parent, child):
        self.matchdict = matchdict
        self.parent = copy.deepcopy(parent)
        self.child = copy.deepcopy(child)

    def as_dict(self):
        return self.matchdict

    def __str__(self):
        return str(self.matchdict)

    def only_qm(self):
        matchdict = self.matchdict
        parent = self.parent
        child = self.child
        kwlist = (
            ('BONDS_INC_HYDROGEN'        , 3, 'NBONH' ),
            ('BONDS_WITHOUT_HYDROGEN'    , 3, 'MBONA' ),
            ('ANGLES_INC_HYDROGEN'       , 4, 'NTHETH'),
            ('ANGLES_WITHOUT_HYDROGEN'   , 4, 'MTHETA'),
            ('DIHEDRALS_INC_HYDROGEN'    , 5, 'NPHIH' ),
            ('DIHEDRALS_WITHOUT_HYDROGEN', 5, 'MPHIA' ),
        )
        for kw, nunit, ptr in kwlist:
            value_ptr = Prmtop.pointer.index(ptr)
            newprm = parent._filter_bonds(matchdict, kw, nunit)
            parent.records[kw] = newprm
            parent.records['POINTERS'][value_ptr] = len(newprm) // nunit

        kwlist = (
            ('BONDS_INC_HYDROGEN'        , 'BOND_FORCE_CONSTANT'    , 3, 'NUMBND'),
            ('BONDS_WITHOUT_HYDROGEN'    , 'BOND_FORCE_CONSTANT'    , 3, 'NUMBND'),
            ('BONDS_INC_HYDROGEN'        , 'BOND_EQUIL_VALUE'       , 3, 'NUMBND'),
            ('BONDS_WITHOUT_HYDROGEN'    , 'BOND_EQUIL_VALUE'       , 3, 'NUMBND'),
            ('ANGLES_INC_HYDROGEN'       , 'ANGLE_FORCE_CONSTANT'   , 4, 'NUMANG'),
            ('ANGLES_WITHOUT_HYDROGEN'   , 'ANGLE_FORCE_CONSTANT'   , 4, 'NUMANG'),
            ('ANGLES_INC_HYDROGEN'       , 'ANGLE_EQUIL_VALUE'      , 4, 'NUMANG'),
            ('ANGLES_WITHOUT_HYDROGEN'   , 'ANGLE_EQUIL_VALUE'      , 4, 'NUMANG'),
            ('DIHEDRALS_INC_HYDROGEN'    , 'DIHEDRAL_FORCE_CONSTANT', 5, 'NPTRA' ),
            ('DIHEDRALS_WITHOUT_HYDROGEN', 'DIHEDRAL_FORCE_CONSTANT', 5, 'NPTRA' ),
            ('DIHEDRALS_INC_HYDROGEN'    , 'DIHEDRAL_PERIODICITY'   , 5, 'NPTRA' ),
            ('DIHEDRALS_WITHOUT_HYDROGEN', 'DIHEDRAL_PERIODICITY'   , 5, 'NPTRA' ),
            ('DIHEDRALS_INC_HYDROGEN'    , 'DIHEDRAL_PHASE'         , 5, 'NPTRA' ),
            ('DIHEDRALS_WITHOUT_HYDROGEN', 'DIHEDRAL_PHASE'         , 5, 'NPTRA' ),
            ('DIHEDRALS_INC_HYDROGEN'    , 'SCEE_SCALE_FACTOR'      , 5, 'NPTRA' ),
            ('DIHEDRALS_WITHOUT_HYDROGEN', 'SCEE_SCALE_FACTOR'      , 5, 'NPTRA' ),
            ('DIHEDRALS_INC_HYDROGEN'    , 'SCNB_SCALE_FACTOR'      , 5, 'NPTRA' ),
            ('DIHEDRALS_WITHOUT_HYDROGEN', 'SCNB_SCALE_FACTOR'      , 5, 'NPTRA' ),
        )
        for kw, value_kw, nunit, ptr in kwlist:
            value_ptr = Prmtop.pointer.index(ptr)
            modprm, parent_value = self._mod_prm(kw, value_kw, nunit)
            parent.records[kw] = modprm
            parent.records[value_kw] = parent_value
            parent.records['POINTERS'][value_ptr] = len(parent_value) // nunit

        self._mod_vdw()
        self._update_iac()
        self._update_cn('LENNARD_JONES_ACOEF')
        self._update_cn('LENNARD_JONES_BCOEF')

        # put vdw parameter 0 if the interaction includes MM atom
        parent._update_mm_iac(matchdict)
        parent._extend_ico()
        Prmtop._extend_cn(parent.records['LENNARD_JONES_ACOEF'])
        Prmtop._extend_cn(parent.records['LENNARD_JONES_BCOEF'])

        return parent

    def _mod_prm(self, kw, value_kw, nunit):
        matchdict = self.matchdict
        parent = self.parent
        child = self.child

        # matchdict: key -> qm, value -> mm
        parmunit = list(group_by(parent.records[kw], nunit))
        parent_value = copy.copy(parent.records[value_kw]) # will be appended
        child_value = child.records[value_kw]
        linkatoms_in_mm = Prmtop._linkatom_in_mm(parent, child, matchdict)

        child_unit = Prmtop._prmlist_to_unit(child, kw, nunit)
        child_sgn = Prmtop._get_child_sgn(child, kw, nunit)

        modparm = []
        for u in parmunit:
            if (abs(u[0]) in linkatoms_in_mm
                or abs(u[-2]) in linkatoms_in_mm):
                parent_value.append(Prmtop._linkatom_unit(child_unit, child_value, u, matchdict))
                modparm.extend(Prmtop._linkatom_parm(u, child_sgn, len(parent_value), matchdict))
            else:
                modparm.extend(u)
        return modparm, parent_value

    def _mod_vdw(self):
        matchdict = self.matchdict
        parent = self.parent
        child = self.child

        linkatoms = UpdatePrmtop._linkatoms(matchdict, parent, child)
        for _ in linkatoms:
            parent._extend_ico()

    def _update_cn(self, kw):
        matchdict = self.matchdict
        parent = self.parent
        child = self.child

        parent_cn = copy.copy(parent.records[kw])
        child_cn = child.records[kw]
        parent_iac = parent.records['ATOM_TYPE_INDEX']
        child_iac = child.records['ATOM_TYPE_INDEX']
        parent_ico_2d = parent._ico_2d()
        linkatoms = UpdatePrmtop._linkatoms(matchdict, parent, child)
        for ich, ipr in linkatoms:
            parent_ntypes = Prmtop._ntype_from_cn(parent_cn)
            Prmtop._extend_cn(parent_cn)
            itypepr = parent_ntypes # new atom type
            itypech = child_iac[ich] - 1
            for jch, jpr in matchdict.items():
                jtypepr = parent_iac[jpr] - 1
                jtypech = child_iac[jch] - 1
                ptr2cnpr = parent_ico_2d[(itypepr, jtypepr)] - 1
                ptr2cnch = parent_ico_2d[(itypech, jtypech)] - 1
                parent_cn[ptr2cnpr] = child_cn[ptr2cnch]
        parent.records[kw] = parent_cn


    @staticmethod
    def _linkatoms(matchdict, parent, child):
        parent_atoms = parent.get_atoms()
        child_atoms = child.get_atoms()
        child_iac = child.records['ATOM_TYPE_INDEX']
        seen_child_iac = set()
        linkatoms = []
        for ich, ipr in matchdict.items():
            is_i_linkatom = (parent_atoms[ipr] != 'H' and child_atoms[ich] == 'H')
            if not is_i_linkatom: continue
            itypech = child_iac[ich] - 1
            if itypech in seen_child_iac: continue
            seen_child_iac.add(itypech)
            #yield (ich, ipr)
            linkatoms.append((ich, ipr))
        return linkatoms

    def _update_iac(self):
        matchdict = self.matchdict
        parent = self.parent
        child = self.child

        parent_atoms = parent.get_atoms()
        child_atoms = child.get_atoms()
        child_iac = child.records['ATOM_TYPE_INDEX']
        parent_iac = parent.records['ATOM_TYPE_INDEX']
        seen_child_iac = {}
        linkatoms = []
        parent_ntype = max(parent_iac)
        for ich, ipr in matchdict.items():
            is_i_linkatom = (parent_atoms[ipr] != 'H' and child_atoms[ich] == 'H')
            if not is_i_linkatom: continue
            itypech = child_iac[ich] - 1
            if itypech not in seen_child_iac:
                parent_ntype += 1
                seen_child_iac[itypech] = parent_ntype
            parent_iac[ipr] = parent_ntype

class Ico_2d:
    def __init__(self, ico_2d):
        self.ico_2d = ico_2d

    @classmethod
    def from_ico(cls, ico):
        ntypes = Ico_2d.ntype_from_ico(ico)
        indice = it.product(range(ntypes), range(ntypes))
        ico_2d = {(i, j): ico[i * ntypes + j] for i, j in indice}
        return Ico_2d(ico_2d)

    def ico_1d(self):
        parent_ico_2d = self.ico_2d
        parent_ntypes = self.ntype()
        indice = it.product(range(parent_ntypes), range(parent_ntypes))
        parent_ico = [parent_ico_2d[(i, j)] for i, j in indice]
        return parent_ico

    def extend_ico_2d(self):
        ico_2d = self.ico_2d
        ntypes = self.ntype() + 1 # extended
        itype = ntypes - 1
        for jtype in range(ntypes):
            new_ico = itype * (itype + 1) // 2 + jtype + 1
            ico_2d[(itype, jtype)] = new_ico
            ico_2d[(jtype, itype)] = new_ico
        return Ico_2d(ico_2d)

    def ntype(self):
        return round(sqrt(len(self.ico_2d)))

    @staticmethod
    def ntype_from_ico(ico):
        # ico should be iterable
        # Both ico_1d and ico_2d should be accepted.
        return round(sqrt(len(ico)))

    @staticmethod
    def extend_ico(ico_1d):
        return Ico_2d.from_ico(ico_1d).extend_ico_2d().ico_1d()

    def __getitem__(self, indice):
        return self.ico_2d[indice]

class Prmtop:
    pointer = [
        'NATOM',    'NTYPES', 'NBONH',  'MBONA',  'NTHETH', 'MTHETA',
        'NPHIH',    'MPHIA',  'NHPARM', 'NPARM',  'NNB',    'NRES',
        'NBONA',    'NTHETA', 'NPHIA',  'NUMBND', 'NUMANG', 'NPTRA',
        'NATYP',    'NPHB',   'IFPERT', 'NBPER',  'NGPER',  'NDPER',
        'MBPER',    'MGPER',  'MDPER',  'IFBOX',  'NMXRS',  'IFCAP',
        'NUMEXTRA', 'NCOPY', ]

    def __init__(self, filename):
        with open(filename, r'r') as f:
            cast_funcs = {'a': str, 'i': int, 'e': float}
            self.version = None
            self.records = {}
            self.formats = {}
            self.keywords = [] # keep keyword order
            nelem, cast, width = None, None, None
            record = None # alias of self.records[flag]
            for l in f:
                if l.rstrip() == r'':
                    pass # skip blank line
                elif l[0] != (r'%'):
                    record += [cast(l[s:s+width])
                        for s in range(0, len(l)-1, width)]
                elif re.match(r'%VERSION', l):
                    self.version = l
                elif re.match(r'%FLAG', l):
                    flag = l.rstrip().replace(r'%FLAG ', '')
                    self.keywords.append(flag)
                    record = self.records[flag] = []
                elif re.match(r'%FORMAT', l):
                    m = re.search(r'\((\d+)([aEI])(\d+)(?:\.(\d+))?\)', l)
                    if m is None:
                        print(l)
                        raise ValueError
                    nelem = int(m.group(1))
                    typ = m.group(2)
                    cast = cast_funcs[typ.lower()]
                    width = int(m.group(3))
                    decimal = m.group(4) # str or None
                    self.formats[flag] = (nelem, typ, width, decimal)
                else:
                    raise ValueError

    def _update_ntype(self):
        ico = self.records['NONBONDED_PARM_INDEX']
        parent_ntypes = round(sqrt(len(ico)))
        self.records['POINTERS'][Prmtop.pointer.index('NTYPES')] = parent_ntypes
        return parent_ntypes

    def _filter_bonds(self, matchdict, kw, nunit):
        parmunit = list(group_by(self.records[kw], nunit))
        qm_in_mm = set(3 * x for x in matchdict.values())
        qmunit = list(it.chain.from_iterable(
            u for u in parmunit if all((abs(i) in qm_in_mm) for i in u[:-1])))
        return qmunit

    @staticmethod
    def _linkatom_in_mm(parent, child, matchdict):
        parent_atoms = parent.get_atoms()
        child_atoms = child.get_atoms()
        linkatoms_in_mm = set(3 * v for k, v in matchdict.items()
            if child_atoms[k] == 'H' and parent_atoms[v] != 'H')
        return linkatoms_in_mm

    @staticmethod
    def _linkatom_parm(u, child_sgn, len_parent_value, matchdict):
        key = tuple(matchdict[abs(i//3)]*3 for i in u[:-1])
        return tuple([abs(idx) * sgn for idx, sgn
            in zip(u[:-1], child_sgn[key])] + [len_parent_value])

    @staticmethod
    def _linkatom_unit(child_unit, child_value, u, matchdict):
        key = tuple(matchdict[abs(i//3)]*3 for i in u[:-1])
        if key in child_unit:
            return child_value[child_unit[key] - 1]
        elif key[::-1] in child_value:
            return child_value[child_unit[key[::-1]] - 1]
        else:
            print('Improper torsion?')
            raise ValueError

    @staticmethod
    def _prmlist_to_unit(child, kw, nunit):
        child_unit = list(group_by(it.chain(
            child.records[kw.replace('WITHOUT', 'INC')],
            child.records[kw.replace('INC', 'WITHOUT')]), nunit))
        child_unit = {tuple(abs(i) for i in l[:-1]): l[-1] for l in child_unit}
        return child_unit

    @staticmethod
    def _get_child_sgn(child, kw, nunit):
    #def _get_child_sgn(child_unit):
        child_unit = list(group_by(it.chain(
            child.records[kw.replace('WITHOUT', 'INC')],
            child.records[kw.replace('INC', 'WITHOUT')]), nunit))
        child_sgn = {}
        for l in child_unit:
            key = tuple(abs(x) for x in l[:-1])
            if key in l:
                print('The key' + str(key) + 'already in child unit.')
                raise ValueError
            child_sgn[key] = tuple(1 if x >= 0 else -1 for x in l[:-1])
        return child_sgn

    @staticmethod
    def _ntype_from_cn(cn):
        # len(cn) = ntype * (ntype + 1) / 2 
        # ntype = (-1 + sqrt(1 + 8 * len(cn))) / 2
        return round((-1 + sqrt(1 + 8 * len(cn))) / 2)

    def _ico_2d(self):
        ico = self.records['NONBONDED_PARM_INDEX']
        return Ico_2d.from_ico(ico)

    def _update_mm_iac(self, matchdict):
        parent_iac = self.records['ATOM_TYPE_INDEX']
        natom = self.records['POINTERS'][Prmtop.pointer.index('NATOM')]
        parent_ntypes = self.records['POINTERS'][Prmtop.pointer.index('NTYPES')] + 1
        qmatoms = set(matchdict.values())
        mmatoms = set(range(natom)) - qmatoms
        for iatom in mmatoms:
            parent_iac[iatom] = parent_ntypes
        self.records['ATOM_TYPE_INDEX'] = parent_iac

    def _extend_ico(self):
        ico = self.records['NONBONDED_PARM_INDEX']
        extd = Ico_2d.extend_ico(ico)
        self.records['NONBONDED_PARM_INDEX'] = extd
        self._update_ntype()
        return extd

    @staticmethod
    def _extend_cn(parent_cn):
        parent_ntypes = Prmtop._ntype_from_cn(parent_cn)
        parent_cn += [0.0] * (parent_ntypes + 1)
        return parent_cn

    def get_edges(self):
        return [(i // 3, j // 3) for i, j, iparm in
            group_by(it.chain(
            self.records['BONDS_INC_HYDROGEN'],
            self.records['BONDS_WITHOUT_HYDROGEN']), 3)]

    def get_atoms(self):
        re_atom = re.compile('[A-Z][A-Za-z]?')
        return [re_atom.match(x).group(0) for x in self.records['ATOM_NAME']]

    def get_atypes(self):
        return [x.strip() for x in self.records['AMBER_ATOM_TYPE']]

    def __str__(self):
        builder = self.version
        # if TypeError was found, check all lists were flattened.
        for k in self.keywords:
            builder += "%-80s\n" % (r"%FLAG " + k.upper())
            (nelem, typ, width, decimal) = self.formats[k]
            builder += "%-80s\n" % (r"%FORMAT("
                + str(nelem) + typ + str(width)
                + ("" if decimal is None else ("." + decimal))
                + ")")
            fmt = ("%" + str(width)
                + ("." + decimal if decimal else r"")
                + {'a': 's', 'i': 'd', 'e': 'E'}[typ.lower()])
            for i, el in enumerate(self.records[k]):
                if i > 0 and i % nelem == 0:
                    builder += "\n"
                builder += fmt % el
            builder += "\n"
        return builder[:-1] # remove final "\n"

if __name__ == '__main__':
    from sys import argv
    sp = Prmtop(argv[1])
    sp_forest = Forest(
        sp.get_edges(),
        sp.get_atoms())
    sln = Prmtop(argv[2])
    #print(sln)
    sln_forest = Forest(
        sln.get_edges(),
        sln.get_atoms())
    match = ForestMatcher.match(sp_forest, sln_forest)
    update = UpdatePrmtop(match, sln, sp)
    #print(match)
    #print(sln.only_qm(match, sp))
    #print(sln.only_qm(update, sp))
    print(update.only_qm())
    #sln.only_qm(match, sp)
