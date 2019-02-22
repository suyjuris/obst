// Auto-generated OpenGL pointer initialisation, see generate_opengl.py
typedef void (*glBindBuffer_t) (GLenum target, GLuint buffer);
typedef void (*glGenBuffers_t) (GLsizei n, GLuint *buffers);
typedef void (*glBufferData_t) (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
typedef void (*glAttachShader_t) (GLuint program, GLuint shader);
typedef void (*glBindAttribLocation_t) (GLuint program, GLuint index, const GLchar *name);
typedef void (*glCompileShader_t) (GLuint shader);
typedef GLuint (*glCreateProgram_t) (void);
typedef GLuint (*glCreateShader_t) (GLenum type);
typedef void (*glEnableVertexAttribArray_t) (GLuint index);
typedef void (*glGetProgramiv_t) (GLuint program, GLenum pname, GLint *params);
typedef void (*glGetProgramInfoLog_t) (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
typedef void (*glGetShaderiv_t) (GLuint shader, GLenum pname, GLint *params);
typedef void (*glGetShaderInfoLog_t) (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
typedef GLint (*glGetUniformLocation_t) (GLuint program, const GLchar *name);
typedef void (*glLinkProgram_t) (GLuint program);
typedef void (*glShaderSource_t) (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
typedef void (*glUseProgram_t) (GLuint program);
typedef void (*glUniform1f_t) (GLint location, GLfloat v0);
typedef void (*glUniform2f_t) (GLint location, GLfloat v0, GLfloat v1);
typedef void (*glUniform1i_t) (GLint location, GLint v0);
typedef void (*glVertexAttribPointer_t) (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);
typedef void (*glGenerateMipmap_t) (GLenum target);
typedef void (*glBindVertexArray_t) (GLuint array);
typedef void (*glGenVertexArrays_t) (GLsizei n, GLuint *arrays);
glBindBuffer_t glBindBuffer;
glGenBuffers_t glGenBuffers;
glBufferData_t glBufferData;
glAttachShader_t glAttachShader;
glBindAttribLocation_t glBindAttribLocation;
glCompileShader_t glCompileShader;
glCreateProgram_t glCreateProgram;
glCreateShader_t glCreateShader;
glEnableVertexAttribArray_t glEnableVertexAttribArray;
glGetProgramiv_t glGetProgramiv;
glGetProgramInfoLog_t glGetProgramInfoLog;
glGetShaderiv_t glGetShaderiv;
glGetShaderInfoLog_t glGetShaderInfoLog;
glGetUniformLocation_t glGetUniformLocation;
glLinkProgram_t glLinkProgram;
glShaderSource_t glShaderSource;
glUseProgram_t glUseProgram;
glUniform1f_t glUniform1f;
glUniform2f_t glUniform2f;
glUniform1i_t glUniform1i;
glVertexAttribPointer_t glVertexAttribPointer;
glGenerateMipmap_t glGenerateMipmap;
glBindVertexArray_t glBindVertexArray;
glGenVertexArrays_t glGenVertexArrays;

void _platform_init_opengl() {
    glBindBuffer = (glBindBuffer_t)glXGetProcAddress((u8*)"glBindBuffer"); assert(glBindBuffer);
    glGenBuffers = (glGenBuffers_t)glXGetProcAddress((u8*)"glGenBuffers"); assert(glGenBuffers);
    glBufferData = (glBufferData_t)glXGetProcAddress((u8*)"glBufferData"); assert(glBufferData);
    glAttachShader = (glAttachShader_t)glXGetProcAddress((u8*)"glAttachShader"); assert(glAttachShader);
    glBindAttribLocation = (glBindAttribLocation_t)glXGetProcAddress((u8*)"glBindAttribLocation"); assert(glBindAttribLocation);
    glCompileShader = (glCompileShader_t)glXGetProcAddress((u8*)"glCompileShader"); assert(glCompileShader);
    glCreateProgram = (glCreateProgram_t)glXGetProcAddress((u8*)"glCreateProgram"); assert(glCreateProgram);
    glCreateShader = (glCreateShader_t)glXGetProcAddress((u8*)"glCreateShader"); assert(glCreateShader);
    glEnableVertexAttribArray = (glEnableVertexAttribArray_t)glXGetProcAddress((u8*)"glEnableVertexAttribArray"); assert(glEnableVertexAttribArray);
    glGetProgramiv = (glGetProgramiv_t)glXGetProcAddress((u8*)"glGetProgramiv"); assert(glGetProgramiv);
    glGetProgramInfoLog = (glGetProgramInfoLog_t)glXGetProcAddress((u8*)"glGetProgramInfoLog"); assert(glGetProgramInfoLog);
    glGetShaderiv = (glGetShaderiv_t)glXGetProcAddress((u8*)"glGetShaderiv"); assert(glGetShaderiv);
    glGetShaderInfoLog = (glGetShaderInfoLog_t)glXGetProcAddress((u8*)"glGetShaderInfoLog"); assert(glGetShaderInfoLog);
    glGetUniformLocation = (glGetUniformLocation_t)glXGetProcAddress((u8*)"glGetUniformLocation"); assert(glGetUniformLocation);
    glLinkProgram = (glLinkProgram_t)glXGetProcAddress((u8*)"glLinkProgram"); assert(glLinkProgram);
    glShaderSource = (glShaderSource_t)glXGetProcAddress((u8*)"glShaderSource"); assert(glShaderSource);
    glUseProgram = (glUseProgram_t)glXGetProcAddress((u8*)"glUseProgram"); assert(glUseProgram);
    glUniform1f = (glUniform1f_t)glXGetProcAddress((u8*)"glUniform1f"); assert(glUniform1f);
    glUniform2f = (glUniform2f_t)glXGetProcAddress((u8*)"glUniform2f"); assert(glUniform2f);
    glUniform1i = (glUniform1i_t)glXGetProcAddress((u8*)"glUniform1i"); assert(glUniform1i);
    glVertexAttribPointer = (glVertexAttribPointer_t)glXGetProcAddress((u8*)"glVertexAttribPointer"); assert(glVertexAttribPointer);
    glGenerateMipmap = (glGenerateMipmap_t)glXGetProcAddress((u8*)"glGenerateMipmap"); assert(glGenerateMipmap);
    glBindVertexArray = (glBindVertexArray_t)glXGetProcAddress((u8*)"glBindVertexArray"); assert(glBindVertexArray);
    glGenVertexArrays = (glGenVertexArrays_t)glXGetProcAddress((u8*)"glGenVertexArrays"); assert(glGenVertexArrays);
}
