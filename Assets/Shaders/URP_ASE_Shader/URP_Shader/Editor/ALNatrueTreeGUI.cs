using System.Runtime.CompilerServices;
using UnityEditor;
using UnityEngine;

public class ALNatrueTreeGUI : ShaderGUI
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

    private static class Styles
    {

        //Enum
        public static string renderingMode = "RenderingMode";
        public static string cullingMode = "CullingMode";

        public static readonly string[] blendNames = { "Opaque", "Cutout", "Transparent", "PreMultiply" };
        public static readonly string[] cullNames = { "Front", "Back", "Double" };
        public static readonly string[] emissNames = { "Mask", "Map" };
        // texture
        public static GUIContent baseMapText = new GUIContent("Color");
        public static GUIContent ColorMap = new GUIContent("ColorMap");
        public static GUIContent LeafMaskMap = new GUIContent("LeafMaskMap");


        public static GUIContent normalMapText = new GUIContent("Normal");
        public static GUIContent aoMapText = new GUIContent("AO(B)");
        public static GUIContent emissMapTex = new GUIContent("EmissionMap");

        public static GUIContent queueSlider = new GUIContent("Priority");

    }

    //enum
    MaterialProperty blendMode = null;
    MaterialProperty cullMode = null;
    //
    MaterialProperty baseMap = null;
    MaterialProperty baseColor = null;

    MaterialProperty colorMap = null;
    MaterialProperty leafMaskMap = null;
    MaterialProperty barkColor = null;
    MaterialProperty leafColor = null;
    MaterialProperty mapIntensity = null;


    MaterialProperty normalMap = null;
    // MaterialProperty normalScale = null;
    MaterialProperty aoMap = null;
    MaterialProperty emissMap = null;
    MaterialProperty emissiveColor = null;
    MaterialProperty ssscolor = null;
    // MaterialProperty metallic = null;
    MaterialProperty AOStrength = null;
    MaterialProperty cutoff = null;
    MaterialProperty QueueOffset = null;
    MaterialProperty transparentZWrite = null;
    MaterialProperty windToggle = null;
    MaterialProperty windIntensity = null;
    MaterialProperty WindParam = null;


    //Material
    MaterialEditor m_MaterialEditor;

    public void FindProperties(MaterialProperty[] props)
    {
        //Enum
        blendMode = FindProperty("_BlendMode", props);
        cullMode = FindProperty("_CullMode", props);
        //base
        baseMap = FindProperty("_BaseMap", props);
        baseColor = FindProperty("_BaseColor", props);

        colorMap = FindProperty("_ColorMap", props);
        leafMaskMap = FindProperty("_LeafMaskMap", props);

        barkColor = FindProperty("_BarkColor", props);
        leafColor = FindProperty("_LeafColor", props);
        mapIntensity = FindProperty("_MapIntensity", props);

        //normal
        normalMap = FindProperty("_NormalMap", props);
        // normalScale = FindProperty("_NormalScale", props);
        //mase
        aoMap = FindProperty("_AOMap", props);


        ssscolor = FindProperty("_ssscolor", props);
        emissMap = FindProperty("_EmissionMap", props);
        emissiveColor = FindProperty("_EmissionColor", props);
        // metallic = FindProperty("_Metallic", props);

        AOStrength = FindProperty("_AOStrength", props);
        cutoff = FindProperty("_Cutoff", props);

        QueueOffset = FindProperty("_QueueOffset", props);

        transparentZWrite = FindProperty("_TransparentZWrite", props);

        windToggle = FindProperty("_WindToggle", props);
        windIntensity = FindProperty("_WindIntensity", props);
        WindParam = FindProperty("_WindParam", props);
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
        m_MaterialEditor.TexturePropertySingleLine(Styles.ColorMap, colorMap, mapIntensity);


        m_MaterialEditor.TexturePropertySingleLine(Styles.LeafMaskMap, leafMaskMap, leafColor, barkColor);



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
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText, normalMap);
            //keyworld true
            SetMaterialKeyword(material, "_NORMALMAP", true);
        }
        else
        {
            m_MaterialEditor.TexturePropertySingleLine(Styles.normalMapText, normalMap);
            SetMaterialKeyword(material, "_NORMALMAP", false);
        }

        //MSAE 
        EditorGUILayout.Space();
        m_MaterialEditor.TexturePropertySingleLine(Styles.aoMapText, aoMap);

        if (aoMap.textureValue != null)
        {
            m_MaterialEditor.ShaderProperty(AOStrength, "AoStrength");
        }
        else
        {
            AOStrength.floatValue = 1;
        }
        //keyworld
        SetMaterialKeyword(material, "_AOMAP", aoMap.textureValue != null);

        m_MaterialEditor.ShaderProperty(this.ssscolor, "Transmission");
        if (ssscolor.colorValue != Color.black)
        {
            SetMaterialKeyword(material, "_TRANSMISSION", true);
        }
        else
        {
            SetMaterialKeyword(material, "_TRANSMISSION", false);
        }




        EditorGUILayout.Space();
        // Emission 
        m_MaterialEditor.TexturePropertySingleLine(Styles.emissMapTex, emissMap ,emissiveColor);
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

        EditorGUILayout.Space(10);

        //wind 
        m_MaterialEditor.ShaderProperty(windToggle, "windToggle");
        if (windToggle.floatValue != 0)
        {
            m_MaterialEditor.ShaderProperty(windIntensity, "windIntensity", indent);
            m_MaterialEditor.ShaderProperty(WindParam, "X:LeafSpeed Y:LeafTurbulence Z:BarkPulse W:BrakWind", indent);
        }
        // else
        // {
        //     windIntensity.floatValue = 0.0f;
        // }

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



    void Queue(Material material)
    {
        //Render Queue
        EditorGUI.showMixedValue = QueueOffset.hasMixedValue;
        EditorGUI.BeginChangeCheck();
        var mode = EditorGUILayout.IntSlider(Styles.queueSlider, (int)QueueOffset.floatValue, -50, 50);

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