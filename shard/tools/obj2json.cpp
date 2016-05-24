#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <vector>
#include <math.h>

template<typename Type, unsigned int Dim>
class Vector
{
public:
	Type data[Dim];

public:
	Vector()
	{
		for(int i=0; i<Dim; i++)
		{
			data[i] = 0;
		}
	}

	Vector(Type* x)
	{
		for(int i=0; i<Dim; i++)
		{
			data[i] = x[i];
		}
	}
};

typedef Vector<int, 3> Vector3i;
typedef Vector<float, 3> Vector3f;
typedef Vector<float, 2> Vector2f;


struct Mesh
{
	std::vector<Vector3i> faces;
	std::vector<Vector3f> vertices;
	std::vector<Vector3f> normals;
	std::vector<Vector2f> texcoord;
};

const int MAX_NO_VERTICES = 3;
const int INDICES_PER_VERTEX = 3;
const bool normalizeNormals = true;
const std::string json_tab = "  ";

//=================================================================================================
// Check if a character is a decimal
//=================================================================================================
inline bool isDecimal(char c)
{
	return c != 0 && c >= '0' && c <= '9';
}

//=================================================================================================
// Check if a character is a whitespace
//=================================================================================================
inline bool isWhitespace(char c)
{
	return c == ' ' || c=='\t';
}

//=================================================================================================
// Check if a character is a forward slash
//=================================================================================================
inline bool isSlash(char c)
{
	return c == '/';
}

//=================================================================================================
// Converts a character to a integer
//=================================================================================================
inline int charToInt(char c)
{
	return static_cast<int>(c) - static_cast<int>('0');
}

//=================================================================================================
// Consumes a slash
//=================================================================================================
inline bool parseSlash(const char*& str)
{
	if(isSlash(*str))
	{
		str++;
		return true;
	}
	else
	{
		return false;
	}
}

//=================================================================================================
// Consumes all whitespaces
//=================================================================================================
inline bool parseWhitespace(const char*& str)
{
	if(!isWhitespace(*str))
	{
		return false;
	}
	while(isWhitespace(*str))
	{
		str++;
	}
	return true;
}

//=================================================================================================
// Consumes a number
//=================================================================================================
bool parseNumber(const char*& str, int& result)
{
	if (!isDecimal(*str))
	{
		return false;
	}

	result = 0;
	while(isDecimal(*str))
	{
		result = result*10 + charToInt(*str);
		str++;
	}

	return true;
}

//=================================================================================================
// Consumes a vertex definition inside a face definition
//=================================================================================================
bool parseVertex(const char*& str, int& vertex, int& normal, int& texcoord)
{
	if(!parseNumber(str, vertex))
	{
		return false;
	}

	normal = vertex;
	texcoord = vertex;

	if(parseSlash(str))
	{
		parseNumber(str, texcoord);
	}

	if(parseSlash(str))
	{
		parseNumber(str, normal);
	}

	return true;
}

//=================================================================================================
// Consumes a face definition
//=================================================================================================
bool parseFace(const char*& str, Vector3i* indices, int& vertexCount, int maxVertexCount)
{
	parseWhitespace(str);

	vertexCount = 0;

	int indexV = 0, indexN = 0, indexT = 0;

	while(parseVertex(str, indexV, indexN, indexT) && vertexCount < maxVertexCount)
	{
		indices[vertexCount].data[0] = indexV;
		indices[vertexCount].data[1] = indexN;
		indices[vertexCount].data[2] = indexT;

		vertexCount++;

		parseWhitespace(str);
	}

	return true;
}

struct CompareFaces 
{
	bool operator() (const Vector3i &a, const Vector3i &b) const
	{ 
		if (a.data[0] > b.data[0])
		{
			return true;
		}
		if (a.data[0] < b.data[0])
		{
			return false;
		}
		if (a.data[1] > b.data[1])
		{
			return true;
		}
		if (a.data[1] < b.data[1])
		{
			return false;
		}

		return a.data[2] > b.data[2];
	}
};

void loadObj(const std::string& filename, Mesh& result)
{	
	// Clear the result
	result.vertices.clear();
	result.faces.clear();
	result.normals.clear();
	result.texcoord.clear();

	// Open the file
	std::ifstream infile;
	infile.open(filename.c_str());
	if(!infile.is_open())
	{
		throw std::runtime_error("Unable to open file: " + filename);
	}

	std::string line;
	bool hasNormals = false;
	bool hasTextureCoordinates = false;

	// Original mesh data.
	// This might get duplicated if two faces partially share vertex data
	std::vector<Vector3f> vertices;
	std::vector<Vector3f> normals;
	std::vector<Vector2f> texcoord;
	std::map<Vector3i, int, CompareFaces> uniqueVertexMap;

	// Loop over all lines
	int unsupportedTypeWarningsLeft = 10;
	int linecount = 0;
	while(getline(infile, line))
	{
		linecount++;

		// Skip comments and empty lines
		if(line.length()==0 || line[0] == '#')
		{
			continue;
		}

		// Streamed reads are easier
		std::istringstream stream(line);
		std::string type;

		stream >> type;
		if(type == "v")
		{
			// Vertex position data
			Vector3f v;
			stream >> v.data[0] >> v.data[1] >> v.data[2];
			vertices.push_back(v);
		} 
		else if(type == "vn")
		{
			// Vertex normal data
			Vector3f vn;
			stream >> vn.data[0] >> vn.data[1] >> vn.data[2];
			if (normalizeNormals)
			{
				float normalLength = vn.data[0]*vn.data[0] + vn.data[1]*vn.data[1] + vn.data[2]*vn.data[2];
				if (abs(1.0f-normalLength) > 1e-3f && normalLength > 1e-3f)
				{
					vn.data[0] /= normalLength;
					vn.data[1] /= normalLength;
					vn.data[2] /= normalLength;
				}
			}

			hasNormals = true;
			normals.push_back(vn);
		} 
		else if(type == "vt")
		{
			// Vertex texture coordinate data
			Vector2f vt;
			stream >> vt.data[0] >> vt.data[1];
			hasTextureCoordinates = true;
			texcoord.push_back(vt);
		} 
		else if(type == "f")
		{
			// Face data
			int pos = (int) stream.tellg();

			const char* ptr = line.c_str() + pos;

			// Every vertex may supply up to 3 indexes (vertex index, normal index, texcoord index)
			Vector3i loadedIndices[MAX_NO_VERTICES];
			int mappedIndices[MAX_NO_VERTICES];
			int vertexCount;

			if(parseFace(ptr, loadedIndices, vertexCount, MAX_NO_VERTICES))
			{
				// If the face is legal, for every vertex, assign the normal and the texcoord
				for (int i = 0; i < vertexCount; ++i)
				{
					if (uniqueVertexMap.find(loadedIndices[i]) != uniqueVertexMap.end())
					{
						mappedIndices[i] = uniqueVertexMap[loadedIndices[i]];
					}
					else
					{
						mappedIndices[i] = (int) result.vertices.size();
						uniqueVertexMap.insert(std::make_pair(loadedIndices[i], (int) uniqueVertexMap.size()));

						int vertexIndex = loadedIndices[i].data[0] - 1;
						if (vertexIndex < (int) vertices.size())
						{
							result.vertices.push_back(vertices[vertexIndex]);	
						}
						else
						{
							throw std::runtime_error("Unknown vertex specified");
						}

						if (hasNormals)
						{
							int normalIndex = loadedIndices[i].data[1] - 1;

							if (normalIndex < (int) normals.size())
							{
								result.normals.push_back(normals[normalIndex]);	
							}
							else
							{
								throw std::runtime_error("Unknown normal specified");
							}
						}


						if (hasTextureCoordinates)
						{
							int texCoordIndex = loadedIndices[i].data[2] - 1;

							if (texCoordIndex < (int) texcoord.size())
							{
								result.texcoord.push_back(texcoord[texCoordIndex]);	
							}
							else
							{
								throw std::runtime_error("Unknown texture coordinate specified");
							}
						}
					}
				}

				if (vertexCount == 3)
				{
					Vector3i tri(mappedIndices);
					result.faces.push_back(tri);
				}
				else if (vertexCount == 4)
				{
					throw std::runtime_error("OBJ loader: quads are not supported, convert them to triangles");
				}
				else
				{
					throw std::runtime_error("OBJ loader: face with strange number of vertices encountered");
				}
			}
			else
			{
				throw std::runtime_error("OBJ loader: face could not be parsed");
			}
		}
		else if (unsupportedTypeWarningsLeft > 0)
		{
			std::cerr << "Unsupported type in obj: " << type << " at line " << linecount << std::endl;
			unsupportedTypeWarningsLeft--;
			if(unsupportedTypeWarningsLeft==0)
			{
				std::cerr << "Too many warnings about unsupported types, further warnings are suppressed." << std::endl;
			}
		}
	}

	if (!hasNormals)
	{
		throw std::runtime_error("OBJ loader: mesh without normals");
	}

	if (result.texcoord.size()==0)
	{
		Vector2f defaultTexCoord;
		defaultTexCoord.data[0] = 0;
		defaultTexCoord.data[1] = 0;
		result.texcoord.resize(result.vertices.size(), defaultTexCoord);
	}

	if (result.normals.size() != result.vertices.size())
	{
		throw std::runtime_error("OBJ loader: inconsistent number of normals");
	}
	if (result.texcoord.size() != result.vertices.size())
	{
		throw std::runtime_error("OBJ loader: inconsistent number of texture coordinates");
	}
}

template<typename Type, unsigned int Dim>
void writeJSONArray(std::ofstream& stream, const std::vector<Vector<Type,Dim> >& data)
{
	stream << "[";
	for(size_t i=0; i<data.size(); i++)
	{
		for(int d=0; d<Dim; d++)
		{
			stream << data[i].data[d];
			if (d!=Dim-1 || i!=data.size()-1)
			{
				 stream << ", ";
			}
		}
	}
	stream << "]";
}

void writeJSON(const std::string& filename, const Mesh& input)
{
	// Open the file
	std::ofstream outfile;
	outfile.open(filename.c_str());
	if(!outfile.is_open())
	{
		throw std::runtime_error("Unable to open file: " + filename);
	}

	// Write
	outfile << "{" << std::endl;
	outfile << json_tab << "\"numVertices\": " << input.vertices.size() << "," << std::endl;
	outfile << json_tab << "\"numFaces\": " << input.faces.size() << "," << std::endl;

	outfile << json_tab << "\"vertices\": ";
	writeJSONArray(outfile, input.vertices);
	outfile << "," << std::endl;

	outfile << json_tab << "\"normals\": ";
	writeJSONArray(outfile, input.normals);
	outfile << "," << std::endl;

	outfile << json_tab << "\"texcoord\": ";
	writeJSONArray(outfile, input.texcoord);
	outfile << "," << std::endl;

	outfile << json_tab << "\"faces\": ";
	writeJSONArray(outfile, input.faces);
	outfile << std::endl;
	outfile << "}";
}

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		std::cout << "usage: obj2json [input-file]";
		return 0;
	}

	try
	{
		std::string filename_in(argv[1]);
		std::string filename_out(filename_in + ".json");
		Mesh mesh;

		std::cout << "reading " << filename_in << "...";
		loadObj(filename_in, mesh);
		std::cout << " done." << std::endl;

		std::cout << "writing " << filename_out << "...";
		writeJSON(filename_out, mesh);
		std::cout << " done." << std::endl;

		return 0;
	}
	catch(std::runtime_error& e)
	{
		std::cerr << e.what() << std::endl;
		return -1;
	}
	catch(...)
	{
		std::cerr << "unknown exception" << std::endl;
		return -1;
	}
}

