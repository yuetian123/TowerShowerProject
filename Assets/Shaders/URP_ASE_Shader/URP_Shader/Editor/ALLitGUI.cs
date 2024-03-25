using System.Runtime.CompilerServices;
using UnityEditor;
using UnityEngine;

public class ALLitGUI : ShaderGUI
{
    public enum BlendMode
    {
        Opaque,
        Cutout,
        Transparent,
        PreMultiply,
    }
    public enum CullMode
    {
        Back,
        Front,
        Double,
    }
    public enum EmissFormat
    {
        Mask,
        Map,
    }

    private static class Styles
    {

        //Enum
        public static string renderingMode = "RenderingMode";
        public static string cullingMode = "CullingMode";
        public static string emissformat = "EmissFormat";

        public static readonly string[] blendNames = { "Opaque", "Cutout", "Transparent", "PreMultiply" };
        public static readonly string[] cullNames = { "Front", "Back", "Double" };
        public static readonly string[] emissNames = { "Mask", "Map" };
        // texture
        public static GUIContent baseMapText = new GUIContent("Albdo");
        public static GUIContent normalMapText = new GUIContent("Normal");
        public static GUIContent msaeMapTex = new GUIContent("Meta(R) Gloss(G) AO(B) Emiss(A)");


        public static GUIContent emissMapTex = new GUIContent("EmissionMap");
        public static GUIContent smoothnessRemapText = new GUIContent("GlossRef");
        public static GUIContent queueSlider = new GUIContent("Priority");

    }

    //enum
    MaterialProperty blendMode = null;
    MaterialProperty cullMode = null;
    MaterialProperty emissMode = null;
    //
    MaterialProperty baseMap = null;
    MaterialProperty baseColor = null;
    MaterialProperty normalMap = null;
    MaterialProperty normalScale = null;
    MaterialProperty normalMapSwitch = null;
    MaterialProperty maseMap = null;
    MaterialProperty emissMap = null;
    MaterialProperty emissiveColor = null;
    MaterialProperty smoothnessMin = null;
    MaterialProperty smoothnessMax = null;
    MaterialProperty metallic = null;
    MaterialProperty AOStrength = null;
    MaterialProperty cutoff = null;
    MaterialProperty QueueOffset = null;
    MaterialProperty unrealSwitch = null;


    MaterialProperty transparentZWrite = null;
    //Material
    MaterialEditor m_MaterialEditor;

    public void FindProperties(MaterialProperty[] props)
    {
        //Enum
        blendMode = FindProperty("_BlendMode", props);
        cullMode = FindProperty("_CullMode", props);
        emissMode = FindProperty("_EMISSFORMAT", props);
        //base
        baseMap = FindProperty("_BaseMap", props);
        baseColor = FindProperty("_BaseColor", props);
        //normal
        normalMap = FindProperty("_NormalMap", props);
        normalScale = FindProperty("_NormalScale", props);
        normalMapSwitch = FindProperty("_NORMALMAPDXGLSWITCH", props);
        unrealSwitch = FindProperty("_UNREALSWITCH", props);
        //mase
        maseMap = FindProperty("_MASEMap", props);
        emissMap = FindProperty("_EmissionMap", props);
        emissiveColor = FindProperty("_EmissionColor", props);
        smoothnessMin = FindProperty("_SmoothnessMin", props);
        smoothnessMax = FindProperty("_SmoothnessMax", props);
        metallic = FindProperty("_Metallic", props);
        AOStrength = FindProperty("_AOStrength", props);
        cutoff = FindProperty("_Cutoff", props);

        QueueOffset = FindProperty("_QueueOffset", props);

        transparentZWrite = FindProperty("_TransparentZWrite", props);
    }

    public override void OnGUI(MaterialEditor materialEditor, MaterialProperty[] props)
    {
        m_MaterialEditor = materialEditor;
        Material material = materialEditor.target as Material;

        //material.doubleSidedGI = (RenderFace)cullingProp.floatValue != RenderFace.Front;
        // material.doubleSidedGI = true;

        FindProperties(props);
        RenderMode(material);
        ShaderPropertiesGUI(material);

        //override  
        EditorGUILayout.Space(10);
        // m_MaterialEditor.RenderQueueField();     渲染队列 关键字定义
        // m_MaterialEditor.DoubleSidedGIField();   DoubleSidedGI 双面flase 单面 true
        m_MaterialEditor.EnableInstancingField();

        Queue(material);

        // materialEditor.TextureScaleOffsetProperty(props);

    }

    void RenderMode(Material material)
    {
        SetupMaterialWithBlendMode(material, (BlendMode)blendMode.floatValue);

        SetupMaterialWithCullMode(material, (CullMode)cullMode.floatValue);

        SetupMaterialWithemissMode(material, (EmissFormat)emissMode.floatValue);
    }

    public void SetupMaterialWithBlendMode(Material material, BlendMode blendMode)
    {
        switch (blendMode)
        {
            case BlendMode.Opaque:
                material.SetOverrideTag("RenderType", "Opaque");
                SetMaterialKeyword(material, "_ALPHATEST_ON", false);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", false);
                material.SetInt("_ZWrite", 1);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.Zero);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Geometry;
                break;
            case BlendMode.Cutout:
                material.SetOverrideTag("RenderType", "TransparentCutout");

                SetMaterialKeyword(material, "_ALPHATEST_ON", true);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", false);
                material.SetInt("_ZWrite", 1);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.Zero);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.AlphaTest;
                break;
            case BlendMode.Transparent:
                material.SetOverrideTag("RenderType", "Transparent");
                SetMaterialKeyword(material, "_ALPHATEST_ON", false);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", false);
                material.SetInt("_ZWrite", (int)transparentZWrite.floatValue);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.SrcAlpha);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
                break;
            case BlendMode.PreMultiply:
                material.SetOverrideTag("RenderType", "Transparent");
                SetMaterialKeyword(material, "_ALPHATEST_ON", false);
                SetMaterialKeyword(material, "_ALPHAPREMULTIPLY_ON", true);
                material.SetInt("_ZWrite", (int)transparentZWrite.floatValue);
                material.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.One);
                material.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
                material.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent;
                break;
        }
    }

    public void SetupMaterialWithCullMode(Material material, CullMode cullMode)
    {
        switch (cullMode)
        {
            case CullMode.Back:
                material.SetInt("_Cull", (int)UnityEngine.Rendering.CullMode.Back);
                break;
            case CullMode.Front:
                material.SetInt("_Cull", (int)UnityEngine.Rendering.CullMode.Front);
                break;
            case CullMode.Double:
                material.SetInt("_Cull", (int)UnityEngine.Rendering.CullMode.Off);
                break;
        }
    }

    public void SetupMaterialWithemissMode(Material material, EmissFormat emissMode)
    {
        switch (emissMode)
        {
            case EmissFormat.Map:
                SetMaterialKeyword(material, "_EMISSFORMAT_MASK", false);
                SetMaterialKeyword(material, "_EMISSFORMAT_MAP", true);
                break;
            case EmissFormat.Mask:
                SetMaterialKeyword(material, "_EMISSFORMAT_MAP", false);
                SetMaterialKeyword(material, "_EMISSFORMAT_MASK", true);
                break;
        }
    }

    const int indent = 1;
    public void ShaderPropertiesGUI(Material material)
    {
        //混合模式
        BlendModePopup();


        if ((BlendMode)blendMode.floatValue == BlendMode.Cutout)
        {
            m_MaterialEditor.ShaderProperty(cutoff, "Cutoff", indent);
        }
        else if ((BlendMode)blendMode.floatValue == BlendMode.Transparent || (BlendMode)blendMode.floatValue == BlendMode.PreMultiply)
        {
            //深度写入应该关闭
            m_MaterialEditor.ShaderProperty(transparentZWrite, "ZWrite", indent);
        }
        //裁切模型
        CullModePopup(material);

        //行间距
        EditorGUILayout.Space(10);

        //basecolor 
        m_MaterialEditor.TexturePropertySingleLine(Styles.baseMapText, baseMap, baseColor);

        if (material.HasProperty("_MainTex"))
        {
            material.SetTexture("_MainTex", baseMap.textureValue);
            material.SetColor("_Color", baseColor.colorValue);

            var baseMapTiling = baseMap.textureScaleAndOffset;
            material.SetTextureScale("_MainTex", new Vector2(baseMapTiling.x, baseMapTiling.y));
            material.SetTextureOffset("_MainTex", new Vector2(baseMapTiling.z, baseMapTiling.w));
        }

        //normal
        if (normalMap.textureValue != null)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText, normalMap, normalScale);
            m_MaterialEditor.ShaderProperty(normalMapSwitch, "DX/OpenGL Switch", indent);
            material.SetInt("_NORMALMAPDXGLSWITCH", (int)normalMapSwitch.floatValue);
            //keyworld true
            SetMaterialKeyword(material, "_NORMALMAP", normalMap.textureValue != null);
        }
        else
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText, normalMap);
            SetMaterialKeyword(material, "_NORMALMAP", false);
        }

        //MSAE 
        // EditorGUILayout.Space();
        if (unrealSwitch.floatValue != 0 && maseMap.textureValue != null)
        {
            Styles.msaeMapTex.text = "AO(R) Rough(G) Meta(B) Emiss(A)";
            Styles.smoothnessRemapText.text = "RoughRef";
        }
        else
        {
            Styles.msaeMapTex.text = "Meta(R) Gloss(G) AO(B) Emiss(A)";
            Styles.smoothnessRemapText.text = "GlossRef";
        }
        m_MaterialEditor.TexturePropertySingleLine(Styles.msaeMapTex, maseMap, unrealSwitch);
        material.SetInt("_UNREALSWITCH", (int)unrealSwitch.floatValue);
        

        m_MaterialEditor.ShaderProperty(metallic, "Metallic");
        if (maseMap.textureValue != null)
        {
            float sMin = smoothnessMin.floatValue;
            float sMax = smoothnessMax.floatValue;
            EditorGUI.BeginChangeCheck();
            EditorGUILayout.MinMaxSlider(Styles.smoothnessRemapText, ref sMin, ref sMax, 0.0f, 1.0f);

            if (EditorGUI.EndChangeCheck())
            {
                smoothnessMin.floatValue = sMin;
                smoothnessMax.floatValue = sMax;
            }
            m_MaterialEditor.ShaderProperty(AOStrength, "AoStrength");
        }
        else
        {
            m_MaterialEditor.ShaderProperty(smoothnessMax, "Smoothness");
            smoothnessMin.floatValue = 0;
            AOStrength.floatValue = 1;
        }
        //keyworld
        SetMaterialKeyword(material, "_MSAEMAP", maseMap.textureValue != null);

        EditorGUILayout.Space();

        // Emission 
        EmissModePopup();
        if ((EmissFormat)emissMode.floatValue == EmissFormat.Map)
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.emissMapTex, emissMap);
            SetMaterialKeyword(material, "_EMISSFORMAT_MASK", false);
            SetMaterialKeyword(material, "_EMISSFORMAT_MAP", true);
        }
        else
        {
            SetMaterialKeyword(material, "_EMISSFORMAT_MASK", true);
            SetMaterialKeyword(material, "_EMISSFORMAT_MAP", false);
        }

        m_MaterialEditor.ShaderProperty(this.emissiveColor, "Emisscolor");

        SetMaterialKeyword(material, "_EMISSION", emissiveColor.colorValue.maxColorComponent > 0.0);

        if (emissiveColor.colorValue.maxColorComponent > 0.0)
        {
            EditorGUI.BeginChangeCheck();
            // m_MaterialEditor.LightmapEmissionProperty(indent);
            material.globalIlluminationFlags = MaterialGlobalIlluminationFlags.BakedEmissive;

            if (EditorGUI.EndChangeCheck())
            {
                material.globalIlluminationFlags &= ~MaterialGlobalIlluminationFlags.EmissiveIsBlack;
            }
        }
        // tilling and offset
        EditorGUILayout.Space(10);
        m_MaterialEditor.TextureScaleOffsetProperty(baseMap);

        //EditorGUILayout.Space(10);

        // m_MaterialEditor.ShaderProperty(sssToggle, "SSS");
        // if (sssToggle.floatValue != 0)
        // {
        //     m_MaterialEditor.ShaderProperty(sssColor, "SSS颜色", indent);
        // }

        // EditorGUILayout.Space(10);
        // m_MaterialEditor.ShaderProperty(this.rimColor, "边缘光颜色");
        // m_MaterialEditor.ShaderProperty(rimPower, "边缘光范围");
        // Color rimColor = this.rimColor.colorValue;
        // rimColor.a = rimPower.floatValue;
        // this.rimColor.colorValue = rimColor;
        // material.SetMaterialKeyword("_RIM", rimColor.maxColorComponent >= 0.04f);
    }

    void BlendModePopup()
    {
        EditorGUI.showMixedValue = blendMode.hasMixedValue;
        var mode = (BlendMode)blendMode.floatValue;

        EditorGUI.BeginChangeCheck();
        mode = (BlendMode)EditorGUILayout.Popup(Styles.renderingMode, (int)mode, Styles.blendNames);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Rendering Mode");
            blendMode.floatValue = (float)mode;
        }

        EditorGUI.showMixedValue = false;
    }


    void CullModePopup(Material material)
    {
        EditorGUI.showMixedValue = cullMode.hasMixedValue;

        var mode = (CullMode)cullMode.floatValue;

        EditorGUI.BeginChangeCheck();
        mode = (CullMode)EditorGUILayout.Popup(Styles.cullingMode, (int)mode, Styles.cullNames);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Culling Mode");
            cullMode.floatValue = (float)mode;
            //doubleSidedGI
            material.doubleSidedGI = (CullMode)cullMode.floatValue != CullMode.Double;
            // Debug.Log(material.doubleSidedGI);
        }
        EditorGUI.showMixedValue = false;
    }

    void EmissModePopup()
    {
        EditorGUI.showMixedValue = emissMode.hasMixedValue;

        var mode = (EmissFormat)emissMode.floatValue;

        EditorGUI.BeginChangeCheck();
        mode = (EmissFormat)EditorGUILayout.Popup(Styles.emissformat, (int)mode, Styles.emissNames);

        if (EditorGUI.EndChangeCheck())
        {
            m_MaterialEditor.RegisterPropertyChangeUndo("Emiss Format");

            emissMode.floatValue = (float)mode;
        }
        EditorGUI.showMixedValue = false;
    }


    void Queue(Material material)
    {
        //Render Queue
        EditorGUI.showMixedValue = QueueOffset.hasMixedValue;
        EditorGUI.BeginChangeCheck();
        int mode = EditorGUILayout.IntSlider(Styles.queueSlider, (int)QueueOffset.floatValue, -50, 50);
        if (EditorGUI.EndChangeCheck())
        {
            QueueOffset.floatValue = (int)mode;
        }
        EditorGUI.showMixedValue = false;
        material.renderQueue += material.HasProperty("_QueueOffset") ? (int)material.GetFloat("_QueueOffset") : 0;
        //debug queue
        // Debug.Log(material.renderQueue);
    }


    public void SetMaterialKeyword(Material material, string keyWord, bool toggle)
    {
        if (toggle)
            material.EnableKeyword(keyWord);
        else
            material.DisableKeyword(keyWord);
    }

}